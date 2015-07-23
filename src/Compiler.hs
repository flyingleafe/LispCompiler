{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NoImplicitPrelude, TemplateHaskell #-}

module Compiler where

import Prelude
import Prelude.Unicode
import Data.List
import Data.List.Unicode
import Data.Monoid
import Data.Monoid.Unicode
import Data.Maybe
import qualified Data.Map as M
import Control.Lens hiding (Const)
import Control.Monad.State
import Control.Comonad
import Control.Applicative hiding (Const)
import Debug.Trace

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
data CompilerState = CS { _flags :: [Flag]
                        , _functions :: M.Map Name FuncDef
                        , _availableExterns :: [Label]
                        , _usedExterns :: [Label]
                        , _labels :: [Label]
                        , _locals :: [Variable]
                        , _freeVars :: [Variable]
                        }
makeLenses ''CompilerState

{--
  A compiler is something with state what may fail with error,
  much like parser
--}
type Compiler = StateT CompilerState (Either Error)

compile :: [Flag] → [NamedLib] → Source → Either Error Assembler
compile cflags libs prog = do
  (programBody, funcs) ← preprocess prog
  let lbls = map (^.label) funcs
      funcs' = M.fromList $ zip lbls funcs
      importedExterns = concatMap libExterns libs
  asm ← evalStateT (compileM programBody libs) (CS cflags funcs' importedExterns [] [] [] [])
  return $ asm

{--
  Main compile function.

  Makes 2 steps:
  1) Scope analysis and building a scope tables
  2) Making code
--}
compileM :: AExp → [NamedLib] → Compiler Assembler
compileM prog libs = do
  defines ← useAll $ functions.traverse
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
  used ← use usedExterns
  let isAffected :: NamedLib → Bool
      isAffected lib = (not ∘ null) (libExterns lib ∩ used)
      affectedLibs = filter isAffected libs
  case mergeNamedLibs affectedLibs >>= maybeMergeAsms asm of
    Left err → fail $ "Cannot merge needed libraries: " ++ err
    Right asm' → return asm'

flagSet :: Flag → Compiler Bool
flagSet f = use $ flags.hasEl f

getAvailableExterns :: Name → Compiler (Maybe Label)
getAvailableExterns nm = gets $ find (≡ nm) ∘ _availableExterns

-- Use given label if it hasn't been used yet
getFuncLabel :: Name → Compiler Label
getFuncLabel nm = do
  hasLabel ← use $ labels.hasEl nm
  if hasLabel
  then fail $ "Duplicate definition of " ++ nm
  else do
    labels <:~ nm
    return nm

allLabels :: [Label]
allLabels = enumerate "lbl"

-- Get local label which haven't been used yet for sure.
newLocalLabel :: Compiler Label
newLocalLabel = do
  used ← use labels
  let newl = head $ filter (not ∘ (∈ used)) allLabels
  labels <:~ newl
  return newl

{--
  Local variables manipulation
--}

type VarCodeGen = String → Variable → String → CodeBlock

varGetCode :: VarCodeGen
varGetCode dst (Ordinary _) src = CodeBlob [Mov dst src]
varGetCode dst (Enclosed _) src = CodeBlob [Mov "rax" src, Mov dst "[rax]"]

varPutCode :: VarCodeGen
varPutCode src (Ordinary _) dst = CodeBlob [Mov dst src]
varPutCode src (Enclosed _) dst = CodeBlob [Mov "rdx" src, Mov "rax" dst, Mov "[rax]" "rdx"]

localVarPlace :: Name → Compiler (Maybe (Variable, String))
localVarPlace v = do
  locs ← use locals
  return $ do
    i ← indexPacked v locs
    let var = locs !! i
    return (var, "[rbp - " ++ show ((length locs - i) ⋅ 8) ++ "]")

freeVarPlace :: Name → Compiler (Maybe (Variable, String))
freeVarPlace v = do
  fvs ← use freeVars
  return $ do
    i ← indexPacked v fvs
    let var = fvs !! i
    return (var, "[rbx + " ++ show (i ⋅ 8 + 8) ++ "]")

varPlace :: Name → Compiler (Maybe (Variable, String))
varPlace v = do
  lv ← localVarPlace v
  fv ← freeVarPlace v
  return $ lv <|> fv

varAction :: VarCodeGen → Name → String → Compiler (Maybe CodeBlock)
varAction f v pl = do
  vp ← varPlace v
  return $ liftM (uncurry $ f pl) vp

varGet, varPut :: Name → String → Compiler (Maybe CodeBlock)
varGet = varAction varGetCode
varPut = varAction varPutCode

{--
  Translates `FuncDef` to `CodeFunction`
  Compiles function body and make it suitable assembler function body
  In particular, generates code for creating proper stack frame
--}
compileFunction :: FuncDef → Compiler CodeFunction
compileFunction foo = do

  initStackFrame foo
  movArgCode ← moveArguments $ foo^.args
  code       ← compileBody $ foo^.body
  resetStackFrame foo

  let code' = [CodeBlob [Enter (show $ 8 * foo^.nlocals) "0"]] ⊕
              saveRegs ⊕
              movArgCode ⊕
              [LocalLabel "tailcall"] ⊕
              code ⊕
              restoreRegs ⊕
              [CodeBlob [Leave, Ret]]

  return $ CodeFunction (foo^.label) code'

compileMain :: AExp → Compiler CodeFunction
compileMain prog = do
  let prog' = Progn [ (Funcall "memmgr_init" [])
                    , prog
                    , (Funcall "memmgr_free" [])]
  usedExterns <:~ "memmgr_init"
  usedExterns <:~ "memmgr_free"
  compileFunction $ FD "main" [ Ordinary "argc"
                              , Ordinary "argv"
                              ] [] prog' (countLocals prog' + 2)

initStackFrame :: FuncDef → Compiler ()
initStackFrame foo = do
  locals <++~ foo^.args
  freeVars .= foo^.frees

resetStackFrame :: FuncDef → Compiler ()
resetStackFrame foo = do
  locals ~\\> foo^.args
  freeVars .= []

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

moveArguments :: [Variable] → Compiler [CodeBlock]
moveArguments = movArgs 0
    where movArgs _ [] = return []
          movArgs n (v:vs) = do
            (var, dst) ← liftM fromJust $ varPlace $ extract v
            movc ← movArg n var
            rest ← movArgs (n + 1) vs
            return $ movc ⊕ rest

          movArg n var = do
            let (src, preop) = if n < 6 then (argsOrder !! n, [])
                               else ("rax", [CodeBlob [Mov "rax" (argsOrder !! n)]])
                x = extract var
            intr  ← liftM fromJust $ introduceVar x
            pcode ← liftM fromJust $ varPut src x
            return $ intr ⊕ preop ⊕ [pcode]


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

applyBuiltin :: Builtin → [[CodeBlock]] → Compiler [CodeBlock]
applyBuiltin b as = do
  usedExterns <++~ builtinExterns b
  return $ builtinBody b as

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
compileBody (BuiltinCall b as) = case getBuiltin b $ length as of
                                      Nothing → fail "Builtin unknown"
                                      Just b' → (applyBuiltin b') =<< mapM compileBody as
compileBody (Funcall f as) = do
  mfoo ← use $ functions.at f
  mimp ← getAvailableExterns f
  let procFoo :: String → Compiler [CodeBlock]
      procFoo name = do
        as' ← mapM compileBody as
        return $ callingCode name as'
  case (mfoo, mimp) of
   (Just foo, Just foo') → fail $ "Duplicate function. Calling " ++ f ++ " when "
                             ++ "it's imported from somewhere (check your libs) :"
                             ++ (show foo) ++ " " ++ (show foo')
   (Just foo, Nothing) → procFoo $ foo^.label
   (Nothing, Just foo) → usedExterns <:~ foo >> procFoo foo
   (Nothing, Nothing) → fail $ "Undefined function: " ++ f

compileBody (Tailcall as) = do
  as' ← mapM compileBody as
  return $ putArgsTail as' ⊕ [CodeBlob [Jump ".tailcall"]]

compileBody (LambdaCall l as) = do
  as' ← mapM compileBody as
  clp ← compileBody l
  let call = clp ⊕ [CodeBlob [Mov "rbx" "rax", Call "[rax]"]]
  return $ [CodeBlob [Push "rbx"]] ⊕ callingCode' call as' ⊕ [CodeBlob [Pop "rbx"]]

compileBody (Set v e) = do
  eb ← compileBody e
  pl ← varPut v "rax"
  case pl of
    Nothing    → fail $ "Undeclared variable '" ++ v ++ "'"
    Just place → return $ eb ⊕ [place]

compileBody (Let bnd e) = do
                   binds ← mapM bindVar bnd
                   eb ← compileBody e
                   locals ~\\> map fst bnd
                   return $ mconcat binds ⊕ eb

compileBody (Var v) = do
  pl ← varGet v "rax"
  case pl of
    Nothing    → fail $ "Undeclared variable '" ++ v ++ "'"
    Just place → return [place]

compileBody (List []) = return [CodeBlob [Xor "rax" "rax"]]  -- empty list is just a nullpointer
compileBody (List (x:xs)) = do
  first ← compileBody x
  rest ← compileBody (List xs)
  (flip applyBuiltin) [first, rest] $ fromJust $ getBuiltin "cons" 2

compileBody (Closure lbl frs) = do
  freeVals ← mapM (compileBody ∘ Var) frs
  let as = [ [CodeBlob [Mov "rax" ("qword " ++ lbl)]]
           , [CodeBlob [Mov "rax" (show $ length frs)]]
           ] ++ freeVals
  usedExterns <:~ "memmgr_make_closure"
  return $ callingCode "memmgr_make_closure" as

{--
  Local variables indroduction
--}

introduceVar :: Name → Compiler (Maybe [CodeBlock])
introduceVar v = do
  mvp ← varPlace v
  usedExterns <:~ "memmgr_alloc"
  return $ do
    (var, place) ← mvp
    case var of
      Ordinary _ → return []
      Enclosed _ → return [CodeBlob [
                            Push "rdi",
                            Mov "rdi" "8",
                            Call "memmgr_alloc",
                            Mov place "rax",
                            Pop "rdi"
                           ]]

bindVar :: (Variable, AExp) → Compiler [CodeBlock]
bindVar (v, e) = do
  eb ← compileBody e
  locals <:~ v

  let x = extract v
  introduction ← introduceVar x
  varCode      ← varPut x "rax"

  return $ eb ⊕ fromJust introduction ⊕ [fromJust varCode]
