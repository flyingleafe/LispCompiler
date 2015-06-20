{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, OverloadedStrings #-}

module Parser.Asm
      -- ( parseCode
      --            , getSourceFromFile
      --            )
       where

import Prelude hiding (takeWhile, concat)
import Prelude.Unicode
import Assembler
import Control.Monad
import Data.Char (isLower)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 hiding (takeWhile, map)
import Control.Applicative((<|>), (<$>), (<*), (*>), (<$))

(<∨>) :: (s → Bool) → (s → Bool) → s → Bool
(<∨>) a b c = a c ∨ b c

newline :: Parser Char
newline = char '\n'

wspaces, wspaces', newlines, newlines' :: Parser ()
wspaces   = void $ many1 $ char ' '
wspaces'  = void $ many' $ char ' '
newlines  = void $ many1 newline
newlines' = void $ many' newline

lexeme, parens :: Parser a → Parser a
lexeme s = try $ wspaces' >> s
parens s = (char '[') *> s <* (char ']')

-- lineComment parses one distinct comment line with \n on the end
-- lineComments parses ≥0 lineComments with optional \n on the end
-- afterCodeComment parses spaces, ";" and everything after it except \n
lineComment, afterCodeComment :: Parser ByteString
lineComment = lexeme ((string ";;;") <|> string (";;")) *> takeWhile (notInClass "\n")
afterCodeComment = lexeme (string ";" *> takeWhile (notInClass "\n"))
--lineComments = concat <$> sepBy lineComment newlines'

-- parses square brackets and adds them to return value
parens' :: Parser ByteString → Parser ByteString
parens' m = do
  l ← string "["
  middle ← m
  r ← string "]"
  return $ l `append` middle `append` r

-- parseLabel parses labels like "label_cool1"
-- anyLabel parses parseLabel optionally starting at '.'
parseLabel, anyLabel :: Parser ByteString
parseLabel = takeWhile1 $ isAlpha_ascii <∨> isDigit <∨> (≡ '_')
anyLabel = liftM2 append (option "" (string ".")) parseLabel

-- localLabelParser parses local label line, skipping \n's after it
localLabelParser :: Parser CodeBlock
localLabelParser = LocalLabel <$> unpack <$> ((wspaces' >> char '.') >> parseLabel)

-- arg parses argument of command -- register (or like-register), label in
-- brackets or arithmetic operation in brackets like [2*rsi+6].
arg :: Parser ByteString
arg = liftM2 append ((option empty $ choice [string "byte",
                                          string "word",
                                          string "dword",
                                          string "qword"]) <* wspaces')
      ((parens' parseLabel) <|>
        (parens' $ takeWhile (isLower <∨> isDigit <∨> (inClass "+*-") <∨> (≡ ' '))))
      <|>
      takeWhile1 (isLower <∨> isDigit)

instrParserG2 :: (String → String → Instruction) → ByteString → Parser Instruction
instrParserG2 constr s = do
  lexeme $ string s
  wspaces
  s1 ← lexeme arg
  char ','
  wspaces
  s2 ← lexeme arg
  return $ constr (unpack s1) (unpack s2)

instrParserG1G :: (String → Instruction) → Parser ByteString →
                  Parser ByteString → Parser Instruction
instrParserG1G constr p str = do
  lexeme $ str
  wspaces
  s1 ← lexeme p
  return $ constr $ unpack s1

instrParserG1 :: (String → Instruction) → ByteString → Parser Instruction
instrParserG1 constr s = instrParserG1G constr arg $ string s

instrSingle :: Instruction → ByteString → Parser Instruction
instrSingle constr s = constr <$ (wspaces' *> (string s) <* wspaces')

-- instrParses parses exactly one instruction with it's arguments
instrParser :: Parser Instruction
instrParser = instrParserG2 Add "add"
              <|> instrParserG2 Sub "sub"
              <|> instrParserG2 Cmp "cmp"
              <|> instrParserG2 Mov "mov"
              <|> instrParserG2 Test "test"
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
              <|> try (do s1 ← try (lexeme (choice $ map string
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
                          return $ Jcc (unpack s1) (unpack s2))
              <|> instrSingle Leave "leave"
              <|> instrSingle Ret "ret"

-- codeParses parse >0 lines of code, skipping after-code comments
codeParser :: Parser CodeBlock
codeParser = CodeBlob <$> sepBy1 (instrParser <* option empty afterCodeComment)
                                 (wspaces' >> newlines)


-- parses local labels or code blobs separated by newlines or comment lines
-- up to eof!
parseCodeBlocks :: Parser [CodeBlock]
parseCodeBlocks =  (sepBy1 (
                       localLabelParser <|>
                       codeParser) (many1 (
                       void lineComment <|>
                                    newlines)))

parseCode :: ByteString → Result [CodeBlock]
parseCode s = feed (parse (parseCodeBlocks <* (skipSpace >> endOfInput)) s) empty
