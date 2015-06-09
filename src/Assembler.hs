{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Assembler where

import Prelude

type Label = String

data Assembler = Assembler
                 { sections :: [Section]
                 , externLabels :: [Label]
                 , globalLabels :: [Label]
                 }
data Section = DataSec [DataLabel] | BssSec [BssLabel] | TextSec [CodeBlock]
data DataLabel = DataLabelS Label String | DataLabelI Label Int
data BssLabel = BssLabel Label Int
data CodeBlock = LocalLabel Label | CodeBlob [Instruction]
data Instruction = Add String String
                 | Sub String String
                 | Cmp String String
                 | Mov String String
                 | Xor String String
                 | And String String
                 | Or String String
                 | Div String
                 | Mul String
                 | Push String
                 | Pop String
                 | Jump Label
                 | Jcc String Label
                 | Call Label
                 | Ret

tabbed :: String → String
tabbed = (++) "    "

shargs1 :: String → String → String
shargs1 s a = s ++ " " ++ a

shargs2 :: String → String → String → String
shargs2 s a b = s ++ " " ++ a ++ " " ++ b

instance Show DataLabel where
  show (DataLabelS l s) = l ++ ":  db '" ++ s ++ "', 0"
  show (DataLabelI l i) = l ++ ":  dq " ++ show i

instance Show BssLabel where
  show (BssLabel l i) = l ++ ":  resb" ++ show i

instance Show CodeBlock where
  show (LocalLabel l) = tabbed $ "." ++ l
  show (CodeBlob is) = unlines $ map (tabbed . show) is

instance Show Instruction where
  show (Add a b)    = shargs2 "add" a b
  show (Sub a b)    = shargs2 "sub" a b
  show (Cmp a b)    = shargs2 "cmp" a b
  show (Mov a b)    = shargs2 "mov" a b
  show (Xor a b)    = shargs2 "xor" a b
  show (And a b)    = shargs2 "and" a b
  show (Or a b)     = shargs2 "or" a b
  show (Div a)      = shargs1 "div" a
  show (Mul a)      = shargs1 "mul" a
  show (Push a)     = shargs1 "push" a
  show (Pop a)      = shargs1 "pop" a
  show (Jump l)     = shargs1 "jmp" l
  show (Jcc c l)    = shargs1 ("j" ++ c) l
  show (Call l)     = shargs1 "call" l
  show (Ret)        = "ret"

instance Show Section where
  show (DataSec list)  = "\nsection .data\n" ++ (unlines $ map (tabbed . show) list)
  show (BssSec list)   = "\nsection .bss\n"  ++ (unlines $ map (tabbed . show) list)
  show (TextSec list)  = "\nsection .text\n" ++ (unlines $ map show list)

instance Show Assembler where
  show (Assembler s e g) =
    unlines (map ((++) "extern ") e) ++ "\n" ++
    unlines (map ((++) "global ") g) ++ "\n" ++
    unlines (map (((++) "\n\n\n") . show) s)
