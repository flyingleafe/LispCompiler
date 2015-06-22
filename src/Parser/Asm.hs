{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, OverloadedStrings #-}

module Parser.Asm (processAssembler, parseCode) where

import Prelude hiding (takeWhile, concat)
import Prelude.Unicode
import Assembler
import Control.Monad
import Data.Char (isLower)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 hiding (takeWhile, map, foldl,
                                     foldr, null, filter, words, unwords)
import Control.Applicative((<|>), (<$>), (<*), (*>), (<$))

(<∨>) :: (s → Bool) → (s → Bool) → s → Bool
(<∨>) a b c = a c ∨ b c

newline :: Parser Char
newline = char '\n'

wspaces, wspaces', newlines :: Parser ()
wspaces   = void $ many1 $ char ' '
wspaces'  = void $ many' $ char ' '
newlines  = void $ many1 newline

lexeme, lexemef :: Parser a → Parser a
lexeme s = wspaces' *> s
lexemef s = wspaces' *> s <* wspaces'

-- lineComment parses one distinct comment line with \n on the end
-- lineComments parses ≥0 lineComments with optional \n on the end
-- afterCodeComment parses spaces, ";" and everything after it except \n
lineComment, afterCodeComment :: Parser ()
lineComment = void (lexeme ((string ";;;") <|> string (";;")) *> takeWhile (notInClass "\n"))
afterCodeComment = void (lexeme (string ";" *> takeWhile (notInClass "\n")))
--lineComments = concat <$> sepBy lineComment newlines'

trash :: Parser ()
trash = lineComment <|> newlines

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
parseLabel = takeWhile1 $ isAlpha_ascii <∨> isDigit <∨> (inClass "_")
anyLabel = liftM2 append (option "" (string ".")) parseLabel
globalLabel= parseLabel <* char ':'

-- localLabelParser parses local label line, skipping \n's after it
localLabelParser :: Parser CodeBlock
localLabelParser = LocalLabel <$> unpack <$> ((wspaces' >> char '.') >> parseLabel
                                              <* (wspaces' >> option () afterCodeComment))

-- arg parses argument of command -- register (or like-register), label in
-- brackets or arithmetic operation in brackets like [2*rsi+6].
arg :: Parser ByteString
arg = liftM2 append ((option empty $ choice [string "byte",
                                          string "word",
                                          string "dword",
                                          string "qword"]) <* wspaces')
      (parens' $ takeWhile (isLower <∨> isDigit <∨> (inClass "+*_.-") <∨> (≡ ' ')))
      <|>
      liftM2 append (string "0x") (takeWhile $ isDigit <∨> inClass "abcdefABCDEF")
      <|>
      (pack <$> show <$> signed decimal)
      <|>
      parseLabel
      <|>
      takeWhile1 (isLower <∨> isDigit)
      <|> -- register
      liftM2 cons (char '\'') (liftM2 cons (anyChar) (string "'")) -- char literal

instrParserG2 :: (String → String → Instruction) → ByteString → Parser Instruction
instrParserG2 constr s = do
  lexeme $ string s
  wspaces
  s1 ← lexeme arg
  char ','
  wspaces'
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
              <|> instrParserG2 Adc "adc"
              <|> instrParserG2 Sub "sub"
              <|> instrParserG2 Sbb "sbb"
              <|> instrParserG2 Cmp "cmp"
              <|> instrParserG2 Test "test"
              <|> instrParserG2 Mov "mov"
              <|> instrParserG2 Enter "enter"
              <|> instrParserG2 Xor "xor"
              <|> instrParserG2 And "and"
              <|> instrParserG2 Or  "or"
              <|> instrParserG2 Shr "shr"
              <|> instrParserG2 Shl "shl"
              <|> instrParserG2 Lea "lea"
              <|> instrParserG2 Bt  "bt"
              <|> instrParserG2 Btr "btr"
              <|> instrParserG2 Bts "bts"
              <|> instrParserG1 Not "not"
              <|> instrParserG1 Neg "neg"
              <|> instrParserG1 Inc "inc"
              <|> instrParserG1 Dec "dec"
              <|> instrParserG1 Div "div"
              <|> instrParserG1 Mul "mul"
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
              <|> instrSingle Clc "clc"
              <|> instrSingle Pushf "pushf"
              <|> instrSingle Popf "popf"
              <|> instrSingle Leave "leave"
              <|> instrSingle Ret "ret"
              <|> instrSingle Nop "nop"

-- codeParses parse >0 lines of code, skipping after-code comments
codeParser :: Parser CodeBlock
codeParser = CodeBlob <$> sepBy1 (instrParser <* option () afterCodeComment)
                                 (lexeme $ newlines)

-- parses local labels or code blobs separated by newlines or comment lines
-- up to eof!
parseCodeBlocks :: Parser [CodeBlock]
parseCodeBlocks = sepBy1 (localLabelParser <|> codeParser) (many1 trash)


parseGlobal :: Parser Label
parseGlobal = unpack <$> (lexemef $ (string "global " *> lexeme parseLabel))

parseExtern :: Parser Label
parseExtern = unpack <$> (lexemef $ (string "extern " *> lexeme parseLabel))

parseSection :: ByteString → Parser ()
parseSection s = (lexeme $ string "section") >> (void $ lexemef $ string $ "." `append` s)

-- parser a and b in arbitrary order separated by c
sepArb2 :: Parser a → Parser b → Parser c → Parser ([a], [b])
sepArb2 pa pb sep = do
  let p1 = (pa >>= \x → return ([x],[]))
  let p2 = (pb >>= \x → return ([],[x]))
  l ← sepBy (p1 <|> p2) sep
  let (ar, br) = foldl (\(a,b) (c,d) → (a++c,b++d)) ([], []) l
  return $ (ar, br)

-- the same for 3 arguments
-- may be used to parse .text .data .bss in arbitrary order
sepArb3 :: Parser a → Parser b → Parser c → Parser d → Parser ([a], [b], [c])
sepArb3 pa pb pc sep = do
  let p1 = (pa >>= \x → return ([x],[],[]))
  let p2 = (pb >>= \x → return ([],[x],[]))
  let p3 = (pc >>= \x → return ([],[],[x]))
  l ← sepBy (p1 <|> p2 <|> p3) sep
  let (ar, br, cr) = foldl (\(a,b,c) (d,e,f) → (a++d,b++e,c++f)) ([], [], []) l
  return $ (ar, br, cr)

-- parser extern/global items in arbitrary order
parseExGlBlock :: Parser ([Label], [Label])
parseExGlBlock = sepArb2 parseExtern parseGlobal (many1 trash)

parseCodeFunction :: Parser CodeFunction
parseCodeFunction = do
  l ← lexemef globalLabel
  many1 trash
  bl ← parseCodeBlocks
  return $ CodeFunction (unpack l) bl

parseDataLabel :: Parser DataLabel
parseDataLabel = do
  i ← lexeme globalLabel
  wspaces'
  text ← lexemef $ takeWhile1 $ notInClass "\n"
  return $ DataLabel (unpack i) (unwords . words $ unpack text)

parseBssLabel :: Parser BssLabel
parseBssLabel = do
  i ← lexeme globalLabel
  wspaces
  unit ← choice $ map string ["resq", "resd", "resw", "resb"]
  wspaces
  num ← lexemef decimal
  let multiplier "resq" = 8
      multiplier "resd" = 4
      multiplier "resw" = 2
      multiplier "resb" = 1
  return $ BssLabel (unpack i) (multiplier unit * num)

parseAssembler :: Parser Assembler
parseAssembler = do
  many' trash
  (e,g) ← parseExGlBlock
  (if (null e ∧ null g) then many' else many1) trash
  parseSection "text" <* many1 trash
  codefuncs ← sepBy parseCodeFunction (many1 trash)
  many1 trash
  dataS ← option [] ((parseSection "data" <* many1 trash) *> sepBy1 parseDataLabel (many1 trash))
  many1 trash
  bssS ← option [] ((parseSection "bss" <* many1 trash) *> sepBy1 parseBssLabel (many1 trash))
  many' trash
  endOfInput
  return $ Assembler codefuncs dataS bssS e g

parseCode :: ByteString → Result [CodeBlock]
parseCode s = feed (parse (parseCodeBlocks <* (skipSpace >> endOfInput)) s) ""

processAssembler :: ByteString → Either String Assembler
processAssembler s = parseOnly parseAssembler s
