{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NoImplicitPrelude #-}

module Parser.Lisp where

import Prelude hiding (takeWhile)
import Prelude.Unicode
import Data.Monoid.Unicode
import SExp
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BS (c2w)
import Control.Applicative ((<$>), (<|>), (<*), (*>))
import Data.Char (ord)

lexeme, parens :: Parser a → Parser a
lexeme p = p <* skipSpace
parens p = lexeme (char '(') *> lexeme p <* lexeme (char ')')

omitComments :: BS.ByteString → BS.ByteString
omitComments = BS.unlines ∘ map removeComment ∘ BS.lines
    where removeComment = BS.takeWhile (≢ ';')

getSExps :: BS.ByteString → Either String [SExp]
getSExps = parseOnly ((many1 $ skipSpace *> sexp) <* (skipSpace *> endOfInput)) ∘ omitComments

sexp :: Parser SExp
sexp = constexpr <|> var <|>
       parens (quote <|> cond <|> define <|> letexpr <|> lambda <|> list)

builtinId :: Parser Identifier
builtinId = BS.unpack <$> takeWhile1 (inClass "-~/%+*=<>")

ordinaryId :: Parser Identifier
ordinaryId = do
  beg ← takeWhile1 $ isAlpha_ascii
  let isOk c = isAlpha_ascii c ∨ isDigit c ∨ inClass "-_/'" c
  end ← takeWhile isOk
  return $ BS.unpack $ beg ⊕ end

identifier :: Parser Identifier
identifier = lexeme $ builtinId <|> ordinaryId

constexpr :: Parser SExp
constexpr = lexeme $ constInt <|> constChar <|> constStr

constChar, constInt, constStr :: Parser SExp
constChar = do
  string "#\\"
  c ← anyChar
  return $ SConst (ord c)

constInt = SConst <$> signed decimal

constStr = do
  char '"'
  chs ← many' $ satisfy $ not ∘ \c → isEndOfLine (BS.c2w c) ∨ c ≡ '"'
  char '"'
  return $ (SQuote ∘ SList ∘ map (SConst ∘ ord)) chs

var :: Parser SExp
var = do
  name ← identifier
  return $ SVar name

quote :: Parser SExp
quote = do
  lexeme $ string "quote"
  expr ← sexp
  return $ SQuote expr

cond :: Parser SExp
cond = do
  lexeme $ string "if"
  condition ← sexp
  consequence ← sexp
  alternative ← sexp
  return $ SCond condition consequence alternative

define :: Parser SExp
define = do
  lexeme $ string "define" <* space
  name ← identifier
  value ← sexp
  return $ SDefine name value

letexpr :: Parser SExp
letexpr = do
  lexeme $ string "let"
  bindings ← parens $ many1 binding
  body ← sexp
  return $ SLet bindings body

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
  return $ SLambda args body

list :: Parser SExp
list = do
  exprs ← many1 sexp
  return $ SList exprs
