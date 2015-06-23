{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module SExp where

type Identifier = String

data SExp = SConst Int
          | SVar Identifier
          | SQuote SExp
          | SCond SExp SExp SExp
          | SDefine Identifier SExp
          | SLet [(Identifier, SExp)] SExp
          | SList [SExp]
          | SString String
          | SLambda [Identifier] SExp
            deriving (Eq, Show)

type Source = [SExp]

data AExp = Const Int
          | Var Identifier
          | Cond AExp AExp AExp
          | Let [(Identifier, AExp)] AExp
          | BuiltinCall Identifier [AExp]
          | Funcall String [AExp]
          | Tailcall String [AExp]
          | LambdaCall AExp [AExp]
          | List [AExp]
          | Progn [AExp]
          | Closure String [Identifier]
            deriving (Eq, Show)
