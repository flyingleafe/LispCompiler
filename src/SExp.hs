{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module SExp where

type Identifier = String

data SExp = Const Int
          | Var Identifier
          | Quote SExp
          | Cond SExp SExp SExp
          | Define Identifier SExp
          | Progn [SExp]
          | Let [(Identifier, SExp)] SExp
          | List [SExp]
          | Lambda [Identifier] SExp
            deriving Show

type Program = [SExp]

{--
  We have no user macros, but have some standard built-in macros
--}
preprocessSExp :: SExp → SExp
preprocessSExp (Quote s) = Quote $ preprocessSExp s
preprocessSExp (Cond a b c) = Cond (preprocessSExp a) (preprocessSExp b) (preprocessSExp c)
preprocessSExp (Define v s) = Define v $ preprocessSExp s
preprocessSExp (Progn ss) = Progn $ map preprocessSExp ss
preprocessSExp (Let ps s) = let ppair (v, s') = (v, preprocessSExp s')
                            in Let (map ppair ps) $ preprocessSExp s
preprocessSExp (Lambda args s) = Lambda args $ preprocessSExp s
preprocessSExp (List []) = List []
preprocessSExp (List ls@(s:ss)) =
    case s of
      (Var "and") → let wrapIf a b = (Cond a b (Const 0))
                    in foldl wrapIf (Const 1) ss
      (Var "or") → let wrapLetIf b a = (Let [("x", a)] (Cond (Var "x") (Var "x") b))
                   in foldl wrapLetIf (Const 0) ss
      _ → List $ map preprocessSExp ls
preprocessSExp s = s

preprocessProg :: Program → Program
preprocessProg = map preprocessSExp
