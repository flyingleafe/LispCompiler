{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Parser.Asm ( parseCode
                  , getSourceFromFile
                  ) where

import Prelude
import Assembler
import Control.Monad
import System.IO
import Text.ParserCombinators.Parsec
import Control.DeepSeq (rnf)
import Control.Applicative((<$>), (<*), (*>), (<$))

-- parses whitespaces and newlines, ≥0 or >0
wspaces, wspaces', newlines, newlines' :: Parser String
wspaces = many1 $ char ' '
wspaces' = many $ char ' '
newlines = many1 newline
newlines' = many newline

lexeme, parens :: Parser a → Parser a
lexeme s = try $ wspaces' >> s
parens = try . between (char '[') (char ']')

-- lineComment parses one distinct comment line with \n on the end
-- lineComments parses ≥0 lineComments with optional \n on the end
-- afterCodeComment parses spaces, ";" and everything after it except \n
lineComment, lineComments, afterCodeComment :: Parser String
lineComment = lexeme (try (string ";;;") <|> string (";;")) >> many (noneOf ("\n"))
afterCodeComment = lexeme (string ";" *> many (noneOf "\n"))
lineComments = concat <$> many (lineComment <* newlines')

-- parses square brackets and adds them to return value
parens' :: Parser String → Parser String
parens' m = do
  l ← try $ string "["
  middle ← m
  r ← try $ string "]"
  return $ l ++ middle ++ r

-- parseLabel parses labels like "label_cool1"
-- anyLabel parses parseLabel optionally starting at '.'
parseLabel, anyLabel :: Parser String
parseLabel = try $ many1 $ letter <|> digit <|> (char '_')
anyLabel = liftM2 (++) (option "" (string ".")) parseLabel

-- codeParses parse >0 lines of code, skipping after-code comments
codeParser :: Parser CodeBlock
codeParser = CodeBlob <$> (sepEndBy1 (try instrParser <* optional afterCodeComment)
                           (try (wspaces' >> newlines)))

-- localLabelParser parses local label line, skipping \n's after it
localLabelParser :: Parser CodeBlock
localLabelParser = LocalLabel <$> (try (wspaces >> char '.') >> parseLabel
                                   <* (optional $ void newlines))
-- arg parses argument of command -- register (or like-register), label in
-- brackets or arithmetic operation in brackets like [2*rsi+6].
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

instrSingle :: Instruction → String → Parser Instruction
instrSingle constr s = try (constr <$ (wspaces' *> (string s) <* wspaces'))

-- instrParses parses exactly one instruction with it's arguments
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
              <|> instrParserG1 Not "not"
              <|> instrParserG1 Neg "neg"
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
              <|> instrSingle Leave "leave"
              <|> instrSingle Ret "ret"

-- parses local labels or code blobs separated by newlines or comment lines
-- up to eof!
parseCodeBlocks :: Parser [CodeBlock]
parseCodeBlocks = (sepEndBy1 (codeParser <|> try localLabelParser )
                   (lineComments <|> newlines'))
                  <* eof

-- helper
parsem :: Parser a → String → Either ParseError a
parsem b str = parse b "oops" str

-- these two are for getting code from templates
-- they parse the same grammar, one is just wrapping another

--parseTextSec :: String → Either ParseError Section
--parseTextSec = parse (TextSec <$> [] <$> CodeFunction "foo" <$> parseCodeBlocks) "oops :("

parseCode :: String → Either ParseError [CodeBlock]
parseCode = parse parseCodeBlocks "oops :("

--- PARSER END

getSourceFromFile :: String → IO (Either ParseError [CodeBlock])
getSourceFromFile filename = do
  input ← openFile filename ReadMode
  s ← hGetContents $! input
  -- that's needed to prevent file being closed before parser gets lazy string
  rnf s `seq` hClose input
  return $ parseCode s
