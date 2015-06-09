{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module SExp where

import qualified Data.ByteString as BS

data SExp = Const Int
          | Var BS.ByteString
          | Quote SExp
          | Cond SExp SExp SExp
          | Define BS.ByteString SExp
          | List [SExp]
          | Lambda [BS.ByteString] SExp
            deriving Show

type Program = [SExp]