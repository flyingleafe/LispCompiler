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
import LibLoader

type Error = String
type Name = String

{--
  `flags` : List of passed flags
  `functions` : Map of functions by name/label
  `availableExterns` : Labels that are available to the compiler from libs
  `usedExterns` : Extern labels that are needed for compiler to link only used libs
  `labels` : List of labels already used in program
  `locals` : List of current local variables. Variable value is placed
  in [ebp - N], where N = length locals - <index of variable in list>
--}
data CompilerState = CS { flags :: [Flag]
                        , functions :: [(Name, FuncDef)]
                        , availableExterns :: [Label]
                        , usedExterns :: [Label]
                        , labels :: [Label]
                        , locals :: [Name]
                        , freeVars :: [Name]
                        }

{--
  A compiler is something with state what may fail with error,
  much like parser
--}
type Compiler = StateT CompilerState (Either Error)

compile :: [Flag] → [NamedLib] → Source → Either Error Assembler
compile flags libs prog = do
  (programBody, funcs) ← preprocess prog
  let lbls = map label funcs
      funcs' = zip lbls funcs
      importedExterns = concatMap libExterns libs
  asm ← evalStateT (compileM programBody libs) (CS flags funcs' importedExterns [] [] [] [])
  return $ asm

{--
  Main compile function.

  Makes 2 steps:
  1) Scope analysis and building a scope tables
  2) Making code
--}
compileM :: AExp → [NamedLib] → Compiler Assembler
compileM prog libs = do
  defines ← gets $ map snd ∘ functions
  funcs ← mapM compileFunction defines

  let flabels = map cflabel funcs
      code = Assembler funcs [] [] [] flabels

  withoutMain ← flagSet WithoutMain
  if withoutMain
  then do
     linkedCode ← linkLibs libs code
     return linkedCode
  else do
    main ← compileMain prog
    linkedCode ← linkLibs libs code
    return $ addFunction main $ addGlobalLabel "main" linkedCode

linkLibs :: [NamedLib] → Assembler → Compiler Assembler
linkLibs libs asm = do
  used ← gets usedExterns
  let isAffected :: NamedLib → Bool
      isAffected lib = (not . null) ((libExterns lib) `intersect` used)
      affectedLibs = filter isAffected libs
  case mergeNamedLibs affectedLibs >>= maybeMergeAsms asm of
    Left err → fail ("Cannot merge needed libraries: " ++ err)
    Right asm' → return asm'

flagSet :: Flag → Compiler Bool
flagSet f = gets $ (f ∈) ∘ flags

getFunction :: Name → Compiler (Maybe FuncDef)
getFunction nm = gets $ lookup nm ∘ functions

getAvailableExterns :: Name → Compiler (Maybe Label)
getAvailableExterns nm = gets $ find (≡ nm) ∘ availableExterns

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

freeVarPlace :: Name → Compiler (Maybe String)
freeVarPlace v = do
  fvs ← gets freeVars
  return $ do
    i ← elemIndex v fvs
    return $ "[rbx + " ++ show (i ⋅ 8 + 8) ++ "]"

varPlace :: Name → Compiler (Maybe String)
varPlace v = do
  lv ← localVarPlace v
  fv ← freeVarPlace v
  return $ lv <|> fv

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
              saveRegs ⊕
              movArgCode ⊕
              [LocalLabel "tailcall"] ⊕
              code ⊕
              restoreRegs ⊕
              [CodeBlob [Leave, Ret]]

  return $ CodeFunction (label foo) code'

compileMain :: AExp → Compiler CodeFunction
compileMain prog = do
  let prog' = Progn [ (Funcall "memmgr_init" [])
                    , prog
                    , (Funcall "memmgr_free" [])]
  compileFunction $ FD "main" ["argc", "argv"] [] prog' (countLocals prog' + 2)

initStackFrame :: FuncDef → Compiler ()
initStackFrame foo = do
  forM_ (args foo) addLocalVar
  modify $ \cs → cs { freeVars = frees foo }

resetStackFrame :: FuncDef → Compiler ()
resetStackFrame foo = do
  forM_ (args foo) removeLocalVar
  modify $ \cs → cs { freeVars = [] }

saveRegs :: [CodeBlock]
saveRegs = [CodeBlob [Push "rbx"]]

restoreRegs :: [CodeBlock]
restoreRegs = [CodeBlob [Pop "rbx"]]

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

callingCode' :: [CodeBlock] → [[CodeBlock]] → [CodeBlock]
callingCode' call as = pushArgsRegs n ⊕
                     putArguments as ⊕ call ⊕
                     clearStackArgs n ⊕
                     popArgsRegs n
                         where n = length as

callingCode :: Label → [[CodeBlock]] → [CodeBlock]
callingCode foo = callingCode' [CodeBlob [Call foo]]

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
  mimp ← getAvailableExterns f
  let procFoo :: String → Compiler [CodeBlock]
      procFoo name = do
        as' ← mapM compileBody as
        return $ callingCode name as'
      addUsedExtern :: String → Compiler ()
      addUsedExtern name = modify (\cs → cs { usedExterns = name : usedExterns cs })
  case (mfoo, mimp) of
   (Just foo, Just foo') → fail ("Duplicate function. Calling " ++ f ++ " when "
                        ++ "it's imported from somewhere (check your libs) :"
                        ++ (show foo) ++ " " ++ (show foo'))
   (Just foo, Nothing) → procFoo $ label foo
   (Nothing, Just foo) → addUsedExtern foo >> procFoo foo
   (Nothing, Nothing) → fail $ "Undefined function: " ++ f

compileBody (Tailcall as) = do
  as' ← mapM compileBody as
  return $ putArgsTail as' ⊕ [CodeBlob [Jump ".tailcall"]]

compileBody (LambdaCall l as) = do
  as' ← mapM compileBody as
  clp ← compileBody l
  let call = clp ⊕ [CodeBlob [Mov "rbx" "rax", Call "[rax]"]]
  return $ [CodeBlob [Push "rbx"]] ⊕ callingCode' call as' ⊕ [CodeBlob [Pop "rbx"]]

compileBody (Let bnd e) = do
                   binds ← mapM bindVar bnd
                   eb ← compileBody e
                   forM_ bnd $ removeLocalVar ∘ fst
                   return $ mconcat binds ⊕ eb

compileBody (Var v) = do
  pl ← varPlace v
  case pl of
    Nothing → fail $ "Undeclared variable '" ++ v ++ "'"
    Just place → return [CodeBlob [Mov "rax" place]]

compileBody (List []) = return [CodeBlob [Xor "rax" "rax"]]  -- empty list is just a nullpointer
compileBody (List (x:xs)) = do
  first ← compileBody x
  rest ← compileBody (List xs)
  return $ applyBuiltin "cons" [first, rest]

compileBody (Closure lbl frs) = do
  freeVals ← mapM (compileBody ∘ Var) frs
  let as = [ [CodeBlob [Mov "rax" lbl]]
           , [CodeBlob [Mov "rax" (show $ length frs)]]
           ] ++ freeVals
  return $ callingCode "memmgr_make_closure" as

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
