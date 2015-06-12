{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Assembler where

import Prelude
import Control.Monad
import Text.ParserCombinators.Parsec
import Control.Applicative((<$>), (<*), (*>), (<$))

type Label = String

data Assembler = Assembler
                 { sections :: [Section]
                 , externLabels :: [Label]
                 , globalLabels :: [Label]
                 }
data Section = DataSec [DataLabel] | BssSec [BssLabel] | TextSec [CodeBlock]
data DataLabel = DataLabelS Label String | DataLabelI Label Int
data BssLabel = BssLabel Label Int
-- local labels are passed without '.' char at the start
data CodeBlock = LocalLabel Label | CodeBlob [Instruction]
data Instruction = Add String String
                 | Sub String String
                 | Cmp String String
                 | Mov String String
                 | Xor String String
                 | And String String
                 | Or String String
                 | Lea String String
                 | Div String
                 | Mul String
                 | Push String
                 | Pop String
                 | Jump Label
                 | Jcc String Label
                 | Call Label
                 | Ret

tabbed, bigtabbed :: String → String
tabbed = (++) "    "
bigtabbed = (++) "        "

shargs1 :: String → String → String
shargs1 s a = s ++ " " ++ tabbed a

shargs2 :: String → String → String → String
shargs2 s a b = s ++ " " ++ tabbed (a ++ ", " ++ b)

instance Show DataLabel where
  show (DataLabelS l s) = l ++ ":  db '" ++ s ++ "', 0"
  show (DataLabelI l i) = l ++ ":  dq " ++ show i

instance Show BssLabel where
  show (BssLabel l i) = l ++ ":  resb" ++ show i

instance Show CodeBlock where
  show (LocalLabel l) = bigtabbed $ "." ++ l
  show (CodeBlob is) = unlines $ map (bigtabbed . show) is

instance Show Instruction where
  show (Add a b)    = shargs2 "add" a b
  show (Sub a b)    = shargs2 "sub" a b
  show (Cmp a b)    = shargs2 "cmp" a b
  show (Mov a b)    = shargs2 "mov" a b
  show (Xor a b)    = shargs2 "xor" a b
  show (And a b)    = shargs2 "and" a b
  show (Or a b)     = shargs2 "or" a b
  show (Lea a b)    = shargs2 "lea" a b
  show (Div a)      = shargs1 "div" a
  show (Mul a)      = shargs1 "mul" a
  show (Push a)     = shargs1 "push" a
  show (Pop a)      = shargs1 "pop" a
  show (Jump l)     = shargs1 "jmp" l
  show (Jcc c l)    = shargs1 c l
  show (Call l)     = shargs1 "call" l
  show (Ret)        = "ret"

instance Show Section where
  show (DataSec list)  = "\nsection .data\n" ++ (unlines $ map (bigtabbed . show) list)
  show (BssSec list)   = "\nsection .bss\n"  ++ (unlines $ map (bigtabbed . show) list)
  show (TextSec list)  = "\nsection .text\n" ++ (unlines $ map show list)

instance Show Assembler where
  show (Assembler s e g) =
    unlines (map ((++) "extern ") e) ++ "\n" ++
    unlines (map ((++) "global ") g) ++ "\n" ++
    unlines (map (((++) "\n\n\n") . show) s)

-- Parsing

wspaces, wspaces', newlines, newlines' :: Parser String
wspaces = many1 $ char ' '
wspaces' = many $ char ' '
newlines = many1 newline
newlines' = many $ char '\n'

lexeme, parens :: Parser a → Parser a
lexeme s = try $ wspaces' >> s
parens = try . between (char '[') (char ']')

lineComment, lineComments, afterCodeComment :: Parser String
lineComment = lexeme (try (string ";;;") <|> string (";;")) >> many (noneOf ("\n"))
afterCodeComment = lexeme (string ";" *> many (noneOf "\n"))
lineComments = concat <$> many (lineComment <* optional newlines)

parens' :: Parser String → Parser String
parens' m = do
  l ← try $ string "["
  middle ← m
  r ← try $ string "]"
  return $ l ++ middle ++ r

parseLabel, anyLabel :: Parser String
parseLabel = try $ many1 $ letter <|> digit <|> (char '_')
anyLabel = liftM2 (++) (option "" (string ".")) parseLabel

codeParser :: Parser CodeBlock
codeParser = CodeBlob <$> (sepEndBy1 (try instrParser <* optional afterCodeComment)
                           (try (wspaces' >> newlines)))

localLabelParser :: Parser CodeBlock
localLabelParser = LocalLabel <$> (try (wspaces >> char '.') >> parseLabel
                                   <* (optional $ void newlines))

arg :: Parser String
arg = liftM2 (++) (option "" $ choice [try $ string "byte",
                                       try $ string "word",
                                       try $ string "dword",
                                       string "qword"] <* wspaces)
      (try (parens' $ many (lower <|> digit <|> (oneOf "+*- "))) <|>
       (parens' parseLabel))
      <|>
      many (try lower <|> try digit)



instrParserG2 :: (String → String → Instruction) → String → Parser Instruction
instrParserG2 constr s = do
  try $ lexeme $ try $ string s
  spaces
  s1 ← lexeme arg
  char ','
  spaces
  s2 ← lexeme arg
  return $ constr s1 s2

instrParserG1G :: (String → Instruction) → Parser String → Parser String → Parser Instruction
instrParserG1G constr p str = do
  try $ lexeme $ try $ str
  spaces
  s1 ← lexeme p
  return $ constr s1

instrParserG1 :: (String → Instruction) → String → Parser Instruction
instrParserG1 constr s = instrParserG1G constr arg $ string s

instrParser :: Parser Instruction
instrParser = instrParserG2 Add "add"
              <|> instrParserG2 Sub "sub"
              <|> instrParserG2 Cmp "cmp"
              <|> instrParserG2 Mov "mov"
              <|> instrParserG2 Xor "xor"
              <|> instrParserG2 And "and"
              <|> instrParserG2 Or  "or"
              <|> instrParserG2 Lea "lea"
              <|> instrParserG1 Div "div"
              <|> instrParserG1 Mul "mul"
              <|> instrParserG1 Push "push"
              <|> instrParserG1 Pop "pop"
              <|> instrParserG1G Call parseLabel (string "call")
              <|> instrParserG1G Jump anyLabel (string "jmp")
              <|> try (do s1 ← try (lexeme (choice $ map (try . string)
                                        ["jae", "ja", "jbe", "jb", "jcxz",
                                         "jc", "jecxz", "je", "jrcxz",
                                         "jnae", "jna", "jnbe", "jnb",
                                         "jnc", "jne", "jnge", "jng",
                                         "jnle", "jnl", "jno", "jnp",
                                         "jns", "jnz", "jo", "jp", "jge",
                                         "jg", "jle", "jl", "jpe", "jpo",
                                         "js", "jz" ]))
                          wspaces
                          s2 ← anyLabel
                          return $ Jcc s1 s2)
              <|> try (Ret <$ (wspaces' *> (string "ret") <* wspaces'))

parseCodeBlocks :: Parser [CodeBlock]
parseCodeBlocks = (sepEndBy1 (codeParser <|> try localLabelParser )
                   (lineComments <|> newlines'))
                  <* eof
                  -- <* eof
                  --(void newlines <|> eof)

parsem :: Parser a → String → Either ParseError a
parsem b str = parse b "oops" str

-- these two are for getting code from templates

parseTextSec :: String → Either ParseError Section
parseTextSec = parse (TextSec <$> parseCodeBlocks) "oops :("

parseCode :: String → Either ParseError [CodeBlock]
parseCode = parse parseCodeBlocks "oops :("
