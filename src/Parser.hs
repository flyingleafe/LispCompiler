{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NoImplicitPrelude #-}

module Parser where

import Prelude hiding (takeWhile)
import Prelude.Unicode
import Data.Monoid.Unicode
import SExp
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import Control.Applicative ((<|>), (<*), (*>))

lexeme, parens :: Parser a → Parser a
lexeme p = p <* skipSpace
parens p = lexeme (string "(") *> lexeme p <* lexeme (string ")")

sexp :: Parser SExp
sexp = constexpr <|> var <|>
       parens (quote <|> cond <|> define <|> lambda <|> list)

identifier :: Parser BS.ByteString
identifier = do
  let okChar c = isAlpha_ascii c ∨ inClass "_-/'" c
  token ← lexeme $ takeWhile isAlpha_ascii ⊕ takeWhile okChar
  if token ≡ ""
  then fail "identifier"
  else return token

constexpr :: Parser SExp
constexpr = do
  num ← lexeme $ signed decimal
  return $ Const num

var :: Parser SExp
var = do
  name ← identifier
  return $ Var name

quote :: Parser SExp
quote = do
  lexeme $ string "quote"
  expr ← sexp
  return $ Quote expr

cond :: Parser SExp
cond = do
  lexeme $ string "if"
  condition ← sexp
  consequence ← sexp
  alternative ← sexp
  return $ Cond condition consequence alternative

define :: Parser SExp
define = do
  lexeme $ string "define"
  name ← identifier
  value ← sexp
  return $ Define name value

lambda :: Parser SExp
lambda = do
  lexeme $ string "lambda"
  args ← parens $ many' identifier
  body ← sexp
  return $ Lambda args body

list :: Parser SExp
list = do
  exprs ← many1 sexp
  return $ List exprs
