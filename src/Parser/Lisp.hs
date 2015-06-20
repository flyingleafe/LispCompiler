{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NoImplicitPrelude #-}

module Parser.Lisp where

import Prelude hiding (takeWhile)
import Prelude.Unicode
import SExp
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Control.Applicative ((<|>), (<*), (*>))

lexeme, parens :: Parser a → Parser a
lexeme p = p <* skipSpace
parens p = lexeme (char '(') *> lexeme p <* lexeme (char ')')

getSExps :: BS.ByteString → Either String [SExp]
getSExps = parseOnly (many1 sexp)

sexp :: Parser SExp
sexp = constexpr <|> var <|>
       parens (progn <|> quote <|> cond <|> define <|> letexpr <|> lambda <|> list)

identifier :: Parser Identifier
identifier = do
  let okChar c = isAlpha_ascii c ∨ inClass "-~_/%+*='" c
  token ← lexeme $ takeWhile1 okChar
  return $ BS.unpack token

constexpr :: Parser SExp
constexpr = do
  num ← lexeme $ signed decimal
  return $ Const num

var :: Parser SExp
var = do
  name ← identifier
  return $ Var name

progn :: Parser SExp
progn = do
  lexeme $ string "progn"
  exprs ← many1 $ lexeme sexp
  return $ Progn exprs

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
  lexeme $ string "define" <* space
  name ← identifier
  value ← sexp
  return $ Define name value

letexpr :: Parser SExp
letexpr = do
  lexeme $ string "let"
  bindings ← parens $ many1 binding
  body ← sexp
  return $ Let bindings body

binding :: Parser (Identifier, SExp)
binding = parens $ do
            key ← identifier
            val ← sexp
            return (key, val)

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
