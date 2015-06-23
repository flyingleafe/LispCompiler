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

import Settings
import Assembler
import SExp
import Builtins
import LibLoader

type Error = String
type Name = String

{--
  This is the function definition datatype
  It contains everything to compile function into a labeled code block
  Some time after here'll be a tricky field named "closure" also
--}
data Function = Function { fname  :: Name
                         , flabel :: Label
                         , fargs  :: [Name]
                         , fbody  :: SExp
                         , frameSize :: Int
                         }

{--
  `flags` : List of passed flags
  `functions` : Map of global functions by name
  `labels` : List of labels already used in program
  `locals` : List of current local variables. Variable value is placed
  in [ebp - N], where N = length locals - <index of variable in list>
--}
data CompilerState = CS { flags :: [Flag]
                        , functions :: [(Name, Function)]
                        , labels :: [Label]
                        , locals :: [Name]
                        }

{--
  A compiler is something with state what may fail with error,
  much like parser
--}
type Compiler = StateT CompilerState (Either Error)

-- TODO use lib somehow
compile :: [Flag] → Assembler → Program → Either Error Assembler
compile flags libs prog = evalStateT (compileM $ preprocessProg prog) (CS flags [] [] [])

{--
  Main compile function.

  Makes 2 steps:
  1) Scope analysis and building a scope tables
  2) Making code
--}
compileM :: Program → Compiler Assembler
compileM prog = do
  buildScopeTables prog

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
    let mainBody = filter (not ∘ isFunDefinition) prog
    main ← compileBody $ Progn mainBody
    let mainFun = CodeFunction "main" (main ⊕ [CodeBlob [Ret]])
    return $ addFunction mainFun $ addGlobalLabel "main" code

isFunDefinition :: SExp → Bool
isFunDefinition (Define _ (Lambda _ _)) = True
isFunDefinition _ = False

flagSet :: Flag → Compiler Bool
flagSet f = gets $ (f ∈) ∘ flags

labelUsed :: Label → Compiler Bool
labelUsed l = gets $ (l ∈) ∘ labels

useLabel :: Label → Compiler ()
useLabel l = modify $ \cs → cs { labels = l : labels cs }

getFunction :: Name → Compiler (Maybe Function)
getFunction nm = gets $ lookup nm ∘ functions

{--
  Get function label with respect to settings:
  prepend it with `_` if flag is passed
--}
getFuncLabel :: Name → Compiler Label
getFuncLabel nm = do
  isPrefixed ← flagSet LabelPrefixes
  let nm' = if isPrefixed then "_" ++ nm else nm

  hasLabel ← labelUsed nm'
  if hasLabel
  then fail $ "duplicate definition of " ++ nm
  else do
    useLabel nm'
    return nm'

allLabels :: [Label]
allLabels = ["lbl"] ++ concatMap enumerate allLabels
    where enumerate s = map (s ++) digits
          digits = map (:[]) ['0'..'9']
{--
  Get local label which haven't been used yet for sure.
--}
newLocalLabel :: Compiler Label
newLocalLabel = do
  used ← gets labels
  let newl = head $ filter (not ∘ (∈ used)) allLabels
  modify $ \cs → cs { labels = newl : labels cs }
  return newl

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
  Translates `Function` to `CodeFunction`
  Compiles function body and make it suitable assembler function body
  In particular, generates code for creating proper stack frame
--}
compileFunction :: Function → Compiler CodeFunction
compileFunction foo = do
  let movArgCode = moveArguments $ fargs foo

  initStackFrame foo
  code ← compileBody $ fbody foo
  resetStackFrame foo

  let code' = [CodeBlob [Enter (show $ 8 * frameSize foo) "0"]] ⊕
              movArgCode ⊕
              code ⊕
              [CodeBlob [Leave, Ret]]

  return $ CodeFunction (flabel foo) code'

initStackFrame :: Function → Compiler ()
initStackFrame foo = forM_ (fargs foo) addLocalVar

resetStackFrame :: Function → Compiler ()
resetStackFrame foo = forM_ (fargs foo) removeLocalVar


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
compileBody :: SExp → Compiler [CodeBlock]
compileBody (Define _ _) = fail "Defines are not allowed in the body"
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

compileBody (List ((Var f):args)) =
    case getBuiltin f $ length args of
      Just b → body b <$> mapM compileBody args
      Nothing → do
        mfoo ← getFunction f
        case mfoo of
          Just foo → do
                  as ← mapM compileBody args
                  return $
                         pushArgsRegs (length args) ⊕
                         putArguments as ⊕ [CodeBlob [Call $ flabel foo]] ⊕
                         clearStackArgs (length args) ⊕
                         popArgsRegs (length args)
          Nothing → fail $ "Undefined function: " ++ f

compileBody (List ((Const n):_)) = fail $ "'" ++ show n ++"' is not a function."
compileBody (Let bnd e) = do
                   binds ← mapM bindVar bnd
                   eb ← compileBody e
                   return $ mconcat binds ⊕ eb

compileBody (Var v) = do
  pl ← localVarPlace v
  case pl of
    Nothing → fail $ "Undeclared variable '" ++ v ++ "'"
    Just place → return [CodeBlob [Mov "rax" place]]

compileBody _ = fail "unsupported language"

bindVar :: (Name, SExp) → Compiler [CodeBlock]
bindVar (v, e) = do
  eb ← compileBody e
  addLocalVar v
  place ← localVarPlace v
  return $ eb ⊕ [CodeBlob [Mov (fromJust place) "rax"]]

{--
  Performs scope analysis and scope tables building
  TBD
--}
buildScopeTables :: Program → Compiler ()
buildScopeTables prog = do
  cs ← get
  funcs ← mapM toFunc $ filter isFunDefinition prog
  put $ cs { functions = funcs }
      where toFunc (Define nm (Lambda args bod)) = do
                          lbl ← getFuncLabel nm
                          return (nm, Function nm lbl args bod sfSize)
                              where sfSize = length args + countLocals bod

countLocals :: SExp → Int
countLocals (Let binds e) = length binds + countLocals e + (sum $ map (countLocals ∘ snd) binds)
countLocals (Cond a b c) = countLocals a + countLocals b + countLocals c
countLocals (Quote e) = countLocals e
countLocals (List ss) = sum $ map countLocals ss
countLocals (Progn ss) = sum $ map countLocals ss
countLocals _ = 0
