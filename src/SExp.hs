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

-- TODO
checkConsistency :: Program → Either String Program
checkConsistency = Right
