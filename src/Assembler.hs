{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, FlexibleInstances, OverlappingInstances #-}

module Assembler where

import Prelude
import Control.Monad
import Data.Text (strip, unpack, pack)
import Data.Monoid
import System.IO
import Text.ParserCombinators.Parsec
import Control.DeepSeq (rnf)
import Control.Applicative((<$>), (<*), (*>), (<$))

type Label = String

data Assembler = Assembler
                 { textSec :: [CodeFunction]
                 , dataSec :: [DataLabel]
                 , bssSec :: [BssLabel]
                 , externLabels :: [Label]
                 , globalLabels :: [Label]
                 }
data DataLabel = DataLabelS Label String | DataLabelI Label Int
data BssLabel = BssLabel Label Int
data CodeFunction = CodeFunction { cflabel :: Label, cfblocks :: [CodeBlock] }
-- local labels are passed without '.' char at the start
data CodeBlock = LocalLabel Label | CodeBlob [Instruction]
data Instruction = Add String String
                 | Sub String String
                 | Cmp String String
                 | Test String String
                 | Mov String String
                 | Not String
                 | Neg String
                 | Xor String String
                 | And String String
                 | Or String String
                 | Shr String String
                 | Shl String String
                 | Lea String String
                 | Inc String
                 | Dec String
                 | Div String
                 | Mul String
                 | Push String
                 | Pop String
                 | Jump Label
                 | Jcc String Label
                 | Call Label
                 | Enter String String
                 | Leave String
                 | Ret

addFunction :: CodeFunction → Assembler → Assembler
addFunction foo code = code { textSec = foo : textSec code }

addGlobalLabel :: Label → Assembler → Assembler
addGlobalLabel l code = code { globalLabels = l : globalLabels code }

instance Monoid [CodeBlock] where
    mempty = []
    mappend [] b = b
    mappend a [] = a
    mappend a b = case (last a, head b) of
                    (CodeBlob as, CodeBlob bs) → init a ++ (CodeBlob $ as ++ bs) : tail b
                    _ → a ++ b

--- SHOW PART

commandToArgsMargin :: Int
commandToArgsMargin = 8

tabbed :: Int → String → String
tabbed n _ | n < 0 = ""
tabbed n s         = ((iterate (++" ") "") !! n) ++ s

bigtabbed :: String → String
bigtabbed = tabbed 8

shargs1 :: String → String → String
shargs1 s a = s ++ " " ++ tabbed (commandToArgsMargin - length s) a

shargs2 :: String → String → String → String
shargs2 s a b = s ++ " " ++ tabbed (commandToArgsMargin - length s)
                (a ++ ", " ++ b)

instance Show DataLabel where
  show (DataLabelS l s) = l ++ ":  db '" ++ s ++ "', 0"
  show (DataLabelI l i) = l ++ ":  dq " ++ show i

instance Show BssLabel where
  show (BssLabel l i) = l ++ ":  resb" ++ show i

instance Show CodeFunction where
  show (CodeFunction l s) = l ++ ":\n" ++ (unlines $ map show s)

instance Show CodeBlock where
  show (LocalLabel l) = bigtabbed $ "." ++ l
  show (CodeBlob is) = unlines $ map (bigtabbed . show) is

instance Show Instruction where
  show (Add a b)    = shargs2 "add" a b
  show (Sub a b)    = shargs2 "sub" a b
  show (Cmp a b)    = shargs2 "cmp" a b
  show (Test a b)   = shargs2 "test" a b
  show (Mov a b)    = shargs2 "mov" a b
  show (Not a)      = shargs1 "not" a
  show (Neg a)      = shargs1 "neg" a
  show (Xor a b)    = shargs2 "xor" a b
  show (And a b)    = shargs2 "and" a b
  show (Or a b)     = shargs2 "or" a b
  show (Shl a b)    = shargs2 "shl" a b
  show (Shr a b)    = shargs2 "shr" a b
  show (Lea a b)    = shargs2 "lea" a b
  show (Inc a)      = shargs1 "inc" a
  show (Dec a)      = shargs1 "dec" a
  show (Div a)      = shargs1 "div" a
  show (Mul a)      = shargs1 "mul" a
  show (Push a)     = shargs1 "push" a
  show (Pop a)      = shargs1 "pop" a
  show (Jump l)     = shargs1 "jmp" l
  show (Jcc c l)    = shargs1 ("j" ++ c) l
  show (Call l)     = shargs1 "call" l
  show (Ret)        = "ret"

strip' :: String -> String
strip' = unpack . Data.Text.strip . pack

instance Show Assembler where
  show (Assembler t d b e g) = strip' (
    unlines (map ((++) "extern ") e) ++ "\n" ++
    unlines (map ((++) "global ") g) ++ "\n" ++
    "\nsection .text\n" ++ (unlines $ map show t) ++
    "\nsection .data\n" ++ (unlines $ map (bigtabbed . show) d) ++
    "\nsection .bss\n"  ++ (unlines $ map (bigtabbed . show) b))

-- PARSER START

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
              <|> try (Ret <$ (wspaces' *> (string "ret") <* wspaces'))

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
