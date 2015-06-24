{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NoImplicitPrelude #-}

module Compiler where

import Prelude
import Prelude.Unicode
import Data.List
import Data.Monoid
import Data.Monoid.Unicode
import Data.Maybe
import Control.Monad.State
import Control.Applicative hiding (Const)

import Utils
import Settings
import Assembler
import SExp
import Builtins
import Preprocessor

type Error = String
type Name = String

{--
  `flags` : List of passed flags
  `functions` : Map of functions by name/label
  `labels` : List of labels already used in program
  `locals` : List of current local variables. Variable value is placed
  in [ebp - N], where N = length locals - <index of variable in list>
--}
data CompilerState = CS { flags :: [Flag]
                        , functions :: [(Name, FuncDef)]
                        , imports :: [Label]
                        , labels :: [Label]
                        , locals :: [Name]
                        }

{--
  A compiler is something with state what may fail with error,
  much like parser
--}
type Compiler = StateT CompilerState (Either Error)

-- TODO use lib somehow
compile :: [Flag] → Assembler → Source → Either Error Assembler
compile flags libs prog = do
  (programBody, funcs) ← preprocess prog
  let lbls = map label funcs
      funcs' = zip lbls funcs
  asm ← evalStateT (compileM programBody) (CS flags funcs' (map cflabel $ textSec libs) lbls [])
  return $ libs ⊕ asm

{--
  Main compile function.

  Makes 2 steps:
  1) Scope analysis and building a scope tables
  2) Making code
--}
compileM :: AExp → Compiler Assembler
compileM prog = do
  defines ← gets $ map snd ∘ functions
  funcs ← mapM compileFunction defines

  let flabels = map cflabel funcs
      code =
        -- It could be added somehow like this
        -- libs ⊕
        Assembler funcs [] [] [] flabels

  withoutMain ← flagSet WithoutMain
  if withoutMain
  then return code
  else do
    main ← compileFunction $ FD "main" ["argc", "argv"] [] prog (countLocals prog + 2)
    return $ addFunction main $ addGlobalLabel "main" code

flagSet :: Flag → Compiler Bool
flagSet f = gets $ (f ∈) ∘ flags

getFunction :: Name → Compiler (Maybe FuncDef)
getFunction nm = gets $ lookup nm ∘ functions

getImported :: Name → Compiler (Maybe Label)
getImported nm = gets $ find (≡ nm) ∘ imports

{--
  Label manipulation.
--}
labelUsed :: Label → Compiler Bool
labelUsed l = gets $ (l ∈) ∘ labels

useLabel :: Label → Compiler ()
useLabel l = modify $ \cs → cs { labels = l : labels cs }

-- Use given label if it hasn't been used yet
getFuncLabel :: Name → Compiler Label
getFuncLabel nm = do
  hasLabel ← labelUsed nm
  if hasLabel
  then fail $ "Duplicate definition of " ++ nm
  else do
    useLabel nm
    return nm

allLabels :: [Label]
allLabels = enumerate "lbl"

-- Get local label which haven't been used yet for sure.
newLocalLabel :: Compiler Label
newLocalLabel = do
  used ← gets labels
  let newl = head $ filter (not ∘ (∈ used)) allLabels
  useLabel newl
  return newl

{--
  Local variables manipulation
--}
addLocalVar :: Name → Compiler ()
addLocalVar v = modify $ \cs → cs { locals = v : locals cs }

removeLocalVar :: Name → Compiler ()
removeLocalVar v = modify $ \cs → cs { locals = delete v (locals cs) }

localVarPlace :: Name → Compiler (Maybe String)
localVarPlace v = do
  locs ← gets locals
  return $ do
    i ← elemIndex v locs
    return $ "[rbp - " ++ show ((length locs - i) ⋅ 8) ++ "]"

{--
  Translates `FuncDef` to `CodeFunction`
  Compiles function body and make it suitable assembler function body
  In particular, generates code for creating proper stack frame
--}
compileFunction :: FuncDef → Compiler CodeFunction
compileFunction foo = do
  let movArgCode = moveArguments $ args foo

  initStackFrame foo
  code ← compileBody $ body foo
  resetStackFrame foo

  let code' = [CodeBlob [Enter (show $ 8 * nlocals foo) "0"]] ⊕
              movArgCode ⊕
              [LocalLabel "tailcall"] ⊕
              code ⊕
              [CodeBlob [Leave, Ret]]

  return $ CodeFunction (label foo) code'

initStackFrame :: FuncDef → Compiler ()
initStackFrame foo = forM_ (args foo) addLocalVar

resetStackFrame :: FuncDef → Compiler ()
resetStackFrame foo = forM_ (args foo) removeLocalVar


{--
  The following functions generate assembly code for reallocating
  function arguments on stack.

  This may seem very strange and unefficient, and that's indeed unefficient,
  but this is done for 2 reasons:

  1) Caring about spoiling the registers containing arguments is a headache
  2) It's very convenient to treat passed arguments in the same way as local vars
--}
argsOrder :: [String]
argsOrder = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"] ++ revStack 2
    where revStack n = ["[rbp + " ++ show (n ⋅ 8) ++ "]"] ++ revStack (n + 1)

moveArguments :: [Name] → [CodeBlock]
moveArguments = movArgs 0
    where movArgs _ [] = []
          movArgs n (v:vs) =
              let dst = "[rbp - " ++ show ((n + 1)*8) ++ "]"
                  (src, preop) = if n < 6 then (argsOrder !! n, [])
                                 else ("rax", [CodeBlob [Mov "rax" (argsOrder !! n)]])
              in preop ⊕ [CodeBlob [Mov dst src]] ⊕
                 movArgs (n + 1) vs

putArguments :: [[CodeBlock]] → [CodeBlock]
putArguments args = putRegs (take 6 args) ⊕ putStack (drop 6 args)
    where putRegs as = mconcat $ map putReg $ zip as argsOrder
          putReg (ab, dst) = ab ⊕ [CodeBlob [Mov dst "rax"]]
          putStack as = mconcat $ map putSt $ reverse as
          putSt ab = ab ⊕ [CodeBlob [Push "rax"]]

putArgsTail :: [[CodeBlock]] → [CodeBlock]
putArgsTail args = mconcat (map pushArg args) ⊕ popArgs (length args)
    where pushArg a = a ⊕ [CodeBlob [Push "rax"]]
          popArgs n = mconcat $ map popArg $ reverse [1..n]
          popArg k = [CodeBlob [Pop "rax", Mov ("[rbp - " ++ show (k ⋅ 8) ++ "]") "rax"]]

pushArgsRegs :: Int → [CodeBlock]
pushArgsRegs n = [CodeBlob $ map Push $ take (min n 6) argsOrder]

popArgsRegs :: Int → [CodeBlock]
popArgsRegs n = [CodeBlob $ map Pop $ reverse $ take (min n 6) argsOrder]

clearStackArgs :: Int → [CodeBlock]
clearStackArgs n
    | n <= 6 = []
    | otherwise = [CodeBlob [Add "esp" (show $ (n - 6) ⋅ 8)]]

{--
  A function which should compile `SExp` into assembler code.
--}
compileBody :: AExp → Compiler [CodeBlock]
compileBody (Progn ss) = concat <$> mapM compileBody ss
compileBody (Const n) = return [CodeBlob [Mov "rax" (show n)]]
compileBody (Cond i t e) = do
  ib ← compileBody i
  tb ← compileBody t
  eb ← compileBody e
  elseL ← newLocalLabel
  finL ← newLocalLabel
  return $ ib ⊕
         [CodeBlob [Test "rax" "rax", Jcc "je" ("." ++ elseL)]] ⊕
         tb ⊕
         [CodeBlob [Jump ("." ++ finL)], LocalLabel elseL] ⊕
         eb ⊕
         [LocalLabel finL]
compileBody (BuiltinCall b as) = applyBuiltin b <$> mapM compileBody as
compileBody (Funcall f as) = do
  mfoo ← getFunction f
  mimp ← getImported f
  let procFoo name = do
        as' ← mapM compileBody as
        let n = length as
        return $
          pushArgsRegs n ⊕
          putArguments as' ⊕ [CodeBlob [Call name]] ⊕
          clearStackArgs n ⊕
          popArgsRegs n
  case (mfoo, mimp) of
   (Just foo, Just foo') → fail ("Duplicate function. Calling " ++ f ++ " when "
                        ++ "it's imported from somewhere (check your libs) :"
                        ++ (show foo) ++ " " ++ (show foo'))
   (Just foo, Nothing) → procFoo $ label foo
   (Nothing, Just foo) → procFoo foo
   (Nothing, Nothing) → fail $ "Undefined function: " ++ f
compileBody (Tailcall f as) = do
  as' ← mapM compileBody as
  return $ putArgsTail as' ⊕ [CodeBlob [Jump ".tailcall"]]
compileBody (Let bnd e) = do
                   binds ← mapM bindVar bnd
                   eb ← compileBody e
                   forM_ bnd $ removeLocalVar ∘ fst
                   return $ mconcat binds ⊕ eb
compileBody (Var v) = do
  pl ← localVarPlace v
  case pl of
    Nothing → fail $ "Undeclared variable '" ++ v ++ "'"
    Just place → return [CodeBlob [Mov "rax" place]]
compileBody _ = fail "unsupported language"

{--
  Local variables indroduction
--}
bindVar :: (Name, AExp) → Compiler [CodeBlock]
bindVar (v, e) = do
  eb ← compileBody e
  addLocalVar v
  place ← localVarPlace v
  return $ eb ⊕ [CodeBlob [Mov (fromJust place) "rax"]]
