{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NoImplicitPrelude #-}

module Parser.Lisp where

import Prelude hiding (takeWhile)
import Prelude.Unicode
import Data.Monoid.Unicode
import SExp
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BS (c2w)
import Control.Applicative ((<$>), (<|>), (<*), (*>), (<*>))
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
sexp = constexpr <|> var <|> tickquote <|>
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

tickquote :: Parser SExp
tickquote = do
  char '\''
  SQuote <$> sexp

quote :: Parser SExp
quote = do
  lexeme $ string "quote"
  SQuote <$> sexp

cond :: Parser SExp
cond = do
  lexeme $ string "if"
  SCond <$> sexp <*> sexp <*> sexp

define :: Parser SExp
define = do
  lexeme $ string "define" <* space
  SDefine <$> identifier <*> sexp

letexpr :: Parser SExp
letexpr = do
  lexeme $ string "let"
  SLet <$> parens (many1 binding) <*> sexp

binding :: Parser (Identifier, SExp)
binding = parens $ do
            key ← identifier
            val ← sexp
            return (key, val)

lambda :: Parser SExp
lambda = do
  lexeme $ string "lambda"
  SLambda <$> parens (many' identifier) <*> sexp

list :: Parser SExp
list = SList <$> many1 sexp
