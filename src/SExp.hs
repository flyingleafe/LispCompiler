{-# LANGUAGE UnicodeSyntax, OverloadedStrings, DeriveFunctor #-}

module SExp where

import Control.Comonad

type Identifier = String

data SExp = SConst Int
          | SVar Identifier
          | SQuote SExp
          | SCond SExp SExp SExp
          | SDefine Identifier SExp
          | SSet Identifier SExp
          | SLet [(Identifier, SExp)] SExp
          | SList [SExp]
          | SString String
          | SLambda [Identifier] SExp
            deriving (Eq, Show)

type Source = [SExp]


{--
  This is a variable datatype.
  The variable may be either ordinary or enclosed.

  A variable is *ordinary*, if it is not enclosed in any lambda, and therefore
  may be just placed on stack normally.

  If a variable is *enclosed* in lambda, then it should be placed in separate
  location in heap and accessed by pointer, in order to be accessible and alterable
  from any closure.
--}
data GenVar a = Ordinary a | Enclosed a deriving (Eq, Show, Functor)

instance Comonad GenVar where
    extract (Ordinary a) = a
    extract (Enclosed a) = a
    extend f v = Ordinary (f v)

type Variable = GenVar Identifier

data AExp = Const Int
          | Var Identifier
          | Cond AExp AExp AExp
          | Set Identifier AExp
          | Let [(Variable, AExp)] AExp
          | BuiltinCall Identifier [AExp]
          | Funcall String [AExp]
          | Tailcall [AExp]
          | LambdaCall AExp [AExp]
          | List [AExp]
          | Progn [AExp]
          | Closure String [Identifier]
            deriving (Eq, Show)
