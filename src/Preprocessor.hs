{-# LANGUAGE UnicodeSyntax, OverloadedStrings, ViewPatterns, TupleSections, TemplateHaskell, OverlappingInstances #-}
module Preprocessor ( FuncDef(FD)
                    , preprocess
                    , isFunDefinition
                    , listAE
                    , countLocals
                    , findFrees
                    , label
                    , args
                    , frees
                    , body
                    , nlocals
                    ) where

import Prelude.Unicode
import Control.Monad.State.Lazy
import Control.Comonad
import Control.Lens hiding (Const)
import Data.List
import Data.List.Unicode
import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Data.Char (ord)

import Utils
import SExp
import Builtins
import Macros

{--
  This is the function definition datatype
  It contains everything to compile function into a labeled code block
--}
data FuncDef = FD { _label :: String
                  , _args :: [Variable]
                  , _frees :: [Variable]
                  , _body :: AExp
                  , _nlocals :: Int
                  } deriving Show

data PrepState = PS { _funcs :: [FuncDef]
                    , _boundVars :: [Identifier]
                    , _usedLabels :: [String]
                    , _currentFunc :: Maybe String
                    }

type Preproc = StateT PrepState (Either String)
type VarSt = State [Variable]

makeLenses ''FuncDef
makeLenses ''PrepState

isFunDefinition :: SExp → Bool
isFunDefinition (SDefine _ (SLambda _ _)) = True
isFunDefinition _ = False

{--
  View for recursive AExps
  Helps when we need to fetch everything from recursive AExp
--}
listAE :: AExp → Maybe [AExp]
listAE (Cond a b c) = Just [a, b, c]
listAE (BuiltinCall _ ss) = Just ss
listAE (Funcall _ ss) = Just ss
listAE (Tailcall ss) = Just ss
listAE (LambdaCall s ss) = Just (s:ss)
listAE (List ss) = Just ss
listAE (Progn ss) = Just ss
listAE _ = Nothing

findFrees :: AExp → [Identifier]
findFrees = ffr []
    where ffr bnd (Var x) = if x ∈ bnd then [] else [x]
          ffr bnd (Let binds s) =
                       let addBinding (bnd', fr) (x, s') = ( bnd' ∪ [extract x]
                                                           , fr ∪ ffr bnd' s'
                                                           )
                       in let (bnd', fr') = foldl addBinding (bnd, []) binds
                          in fr' ∪ ffr bnd' s
          ffr bnd (listAE → Just ss) = unionMap (ffr bnd) ss
          ffr _ _ = []

findEnclosed :: AExp → VarSt AExp
findEnclosed e@(Closure _ vars) = do
  forM_ vars $ \v → do
    id %≟ exchangeBy (\x → extract x ≡ v) (Enclosed v)
  return e
findEnclosed (Let binds s) = do
                    let bvars = map fst binds
                    id <++~ bvars
                    s' ← findEnclosed s
                    rbinds ← forM (reverse binds) $ \(var, e) → do
                               curs ← get
                               let x    = extract var
                                   mvar = findPacked x curs
                               e' ← findEnclosed e
                               case mvar of
                                 Nothing   → return (var, e')
                                 Just var' → return (var', e')
                    id %= drop (length bvars)
                    return $ Let (reverse rbinds) s'
findEnclosed (Set x s) = Set x <$> findEnclosed s
findEnclosed (Cond a b c) = Cond <$> findEnclosed a <*> findEnclosed b <*> findEnclosed c
findEnclosed (BuiltinCall f ss) = BuiltinCall f <$> mapM findEnclosed ss
findEnclosed (Funcall f ss) = Funcall f <$> mapM findEnclosed ss
findEnclosed (Tailcall ss) = Tailcall <$> mapM findEnclosed ss
findEnclosed (LambdaCall s ss) = LambdaCall <$> findEnclosed s <*> mapM findEnclosed ss
findEnclosed (List ss) = List <$> mapM findEnclosed ss
findEnclosed (Progn ss) = Progn <$> mapM findEnclosed ss
findEnclosed v = return v

findEnclosedFunc :: FuncDef → FuncDef
findEnclosedFunc (FD nm as fr bod nl) = FD nm as' fr' bod' nl
    where (bod', allVars) = runState (findEnclosed bod) []
          as' = map ifEnc as
          fr' = map ifEnc fr
          ifEnc (extract → x) = if x ∈ encs then Enclosed x else Ordinary x
          encs = map extract $ filter isEnc allVars
          isEnc (Enclosed _) = True
          isEnc _ = False

countLocals :: AExp → Int
countLocals (Let binds e) = length binds + countLocals e + (sum $ map (countLocals ∘ snd) binds)
countLocals (listAE → Just ss) = sum $ map countLocals ss
countLocals _ = 0

{--
  Adds function definition into state
  For convenience, calculates and returns list of free variables
  in function body
--}
addFunc :: Identifier → [Identifier] → AExp → Preproc [Identifier]
addFunc nm as bod = do
  labels ← useAll $ funcs.traverse.label
  if nm ∈ labels
  then fail $ "Duplicate definition of function: " ++ nm
  else do
    let fr  = findFrees bod \\ as
        as' = map Ordinary as
        fr' = map Ordinary fr
        nl  = length as + countLocals bod
    funcs <:~ findEnclosedFunc (FD nm as' fr' bod nl)
    return fr

withBounds :: [Identifier] → Preproc a → Preproc a
withBounds vs action = (boundVars <++~ vs) *> action <* (boundVars ~\\> vs)

flabels :: [String]
flabels = enumerate "lambda"

newFLabel :: Preproc String
newFLabel = do
  used ← use usedLabels
  let new = head $ filter (not ∘ (∈ used)) flabels
  useFLabel new
  return new

useFLabel :: String → Preproc ()
useFLabel lbl = do
  hasAlready ← use $ usedLabels.hasEl lbl
  if hasAlready
  then fail $ "Duplicate label usage: '" ++ lbl ++ "'"
  else usedLabels <:~ lbl

{--
  Main preprocessing function.
  Translates SExp into AExp and expands macros if necessary
--}
preproc :: SExp → Preproc AExp
preproc (SDefine _ _) = fail "No non-toplevel non-function defines!"
preproc (SConst n) = return $ Const n
preproc (SVar n) = return $ Var n
preproc (SQuote (SList ss)) = List <$> mapM preproc ss
preproc (SQuote s) = preproc s   -- no other uses for quote yet
preproc (SCond a b c) = Cond <$> preproc a <*> preproc b <*> preproc c
preproc (SSet x s) = Set x <$> preproc s
preproc (SLet bnds s) = withBounds (map fst bnds) $
                        Let <$> mapM ppBind bnds <*> preproc s
                                where ppBind (x, s') = (Ordinary x, ) <$> preproc s'
preproc (SList []) = return $ List []
preproc (SList ((SVar f):as)) =
    if f ≡ "recur"
    then do
         cur ← use currentFunc
         case cur of
           Nothing → fail "Recur lambda call outside function"
           Just cur' → Funcall cur' <$> mapM preproc as
    else
    if hasMacro f (length as)
    then macroexpand f <$> mapM preproc as
    else if hasBuiltin f (length as)
         then BuiltinCall f <$> mapM preproc as
         else do
           hb ← use $ boundVars.hasEl f
           if not hb then Funcall f <$> mapM preproc as
           else LambdaCall (Var f) <$> mapM preproc as

preproc (SList (s:ss)) = LambdaCall <$> preproc s <*> mapM preproc ss
preproc (SString str) = return $ List $ map (Const ∘ ord) str
preproc (SLambda as bod) = do
  lbl ← newFLabel
  currentFunc ?= lbl
  bod' ← withBounds as $ preproc bod
  fr ← addFunc lbl as $ findTailcalls lbl bod'
  currentFunc .= Nothing
  return $ Closure lbl fr

{--
  Find tail calls and mark it as a tail calls
  explicitly to optimize in future
--}
findTailcalls :: Identifier → AExp → AExp
findTailcalls nm (Cond a b c) = Cond a (findTailcalls nm b) (findTailcalls nm c)
findTailcalls nm (Let binds s) = Let binds $ findTailcalls nm s
findTailcalls _ (Progn []) = Progn []
findTailcalls nm (Progn ss) = Progn $ init ss ++ [findTailcalls nm $ last ss]
findTailcalls nm cc@(Funcall nm' s) =
    if nm ≡ nm' then Tailcall s
    else cc
findTailcalls _ s = s

addFunDef :: SExp → Preproc ()
addFunDef (SDefine nm (SLambda as bod)) = do
  currentFunc ?= nm
  bod' ← withBounds as $ preproc bod
  addFunc nm as $ findTailcalls nm bod'
  currentFunc .= Nothing
addFunDef _ = fail "Wrong definition format"

preprocProg :: Source → Preproc AExp
preprocProg ls = do
  let fs = filter isFunDefinition ls
      es = filter (not ∘ isFunDefinition) ls
  forM_ fs addFunDef
  Progn <$> mapM preproc es

preprocess :: Source → Either String (AExp, [FuncDef])
preprocess ss = do
  (ae, PS foos _ _ _) ← runStateT (preprocProg ss) (PS [] [] [] Nothing)
  return (ae, foos)
--}
