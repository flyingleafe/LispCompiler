{-# LANGUAGE UnicodeSyntax, OverloadedStrings, ViewPatterns, TupleSections #-}
module Preprocessor ( FuncDef(..)
                    , preprocess
                    , isFunDefinition
                    , listAE
                    , countLocals
                    , findFrees
                    ) where

import Prelude.Unicode
import Control.Monad.State.Lazy
import Data.List.Unicode
import Control.Applicative ((<$>), (<*>))
import Data.Char (ord)

import Utils
import SExp
import Builtins
import Macros

{--
  This is the function definition datatype
  It contains everything to compile function into a labeled code block
  Some time after here'll be a tricky field named "closure" also
--}
data FuncDef = FD { label :: String
                  , args :: [Identifier]
                  , frees :: [Identifier]
                  , body :: AExp
                  , nlocals :: Int
                  } deriving Show

type Preproc = StateT [FuncDef] (Either String)

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
                       let addBinding (bnd', fr) (x, s') = (bnd' ∪ [x], fr ∪ ffr bnd' s')
                       in let (bnd', fr') = foldl addBinding (bnd, []) binds
                          in fr' ∪ ffr bnd' s
          ffr bnd (listAE → Just ss) = unionMap (ffr bnd) ss
          ffr _ _ = []

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
  present ← gets $ (nm ∈) ∘ map label
  if present then fail $ "Duplicate definition of function: " ++ nm
  else do
    let fr = findFrees bod
        nl = length as + countLocals bod
    modify $ ((FD nm as fr bod nl):)
    return fr

flabels :: [String]
flabels = enumerate "lambda"

newFLabel :: Preproc String
newFLabel = do
  used ← gets $ map label
  let new = head $ filter (not ∘ (∈ used)) flabels
  return new

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
preproc (SLet bnds s) = Let <$> mapM ppBind bnds <*> preproc s
                                where ppBind (x, s') = (x, ) <$> preproc s'
preproc (SList []) = return $ List []
preproc (SList ((SVar f):as)) =
    if hasMacro f (length as)
    then macroexpand f <$> mapM preproc as
    else if hasBuiltin f (length as)
         then BuiltinCall f <$> mapM preproc as
         else Funcall f <$> mapM preproc as
preproc (SList _) = fail "Invalid (unsupported) list"
preproc (SString str) = return $ List $ map (Const ∘ ord) str
preproc (SLambda as bod) = do
  lbl ← newFLabel
  bod' ← preproc bod
  fr ← addFunc lbl as bod'
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
  bod' ← preproc bod
  addFunc nm as $ findTailcalls nm bod'
  return ()
addFunDef _ = fail "Wrong definition format"

preprocProg :: Source → Preproc AExp
preprocProg ls = do
  let fs = filter isFunDefinition ls
      es = filter (not ∘ isFunDefinition) ls
  forM_ fs addFunDef
  Progn <$> mapM preproc es

preprocess :: Source → Either String (AExp, [FuncDef])
preprocess ss = runStateT (preprocProg ss) []
