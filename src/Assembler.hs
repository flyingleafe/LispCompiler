{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, FlexibleInstances, OverlappingInstances #-}

module Assembler where

import Prelude
import Data.Text (strip, unpack, pack)
import Data.Monoid
import Data.Monoid.Unicode
import Data.List (nub)

type Label = String

data Assembler = Assembler
                 { textSec :: [CodeFunction]
                 , dataSec :: [DataLabel]
                 , bssSec :: [BssLabel]
                 , externLabels :: [Label]
                 , globalLabels :: [Label]
                 }
data DataLabel = DataLabel { datalabel :: String, descr :: String } deriving Eq
data BssLabel = BssLabel { bsslabel :: String, n :: Int } deriving Eq
data CodeFunction = CodeFunction { cflabel :: Label, cfblocks :: [CodeBlock] }
-- local labels are passed without '.' char at the start
data CodeBlock = LocalLabel Label | CodeBlob [Instruction]
{--
  To add new instruction, contact volhovm (that's the best solution possible) or:
  1. Add it to datatype
  2. Add it to show instance
  3. Add it to parser (tricky)

  I don't see any viable solution to make the process easier
  (any attempt usually leads to spaghetty effect)
--}
data Instruction = Add   String String
                 | Adc   String String
                 | Sub   String String
                 | Sbb   String String
                 | Cmp   String String
                 | Test  String String
                 | Mov   String String
                 | Enter String String
                 | Xor   String String
                 | And   String String
                 | Or    String String
                 | Shr   String String
                 | Shl   String String
                 | Lea   String String
                 | Bt    String String
                 | Btr   String String
                 | Bts   String String
                 | Not   String
                 | Neg   String
                 | Inc   String
                 | Dec   String
                 | Div   String
                 | Mul   String
                 | Push  String
                 | Pop   String
                 | Call  Label
                 | Jump  Label
                 | Jcc   String Label
                 | Clc
                 | Pushf
                 | Popf
                 | Leave
                 | Ret
                 | Nop

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

instance Monoid Assembler where
  mempty = Assembler [] [] [] [] []
  mappend (Assembler t1 d1 b1 e1 g1) (Assembler t2 d2 b2 e2 g2) =
    Assembler (t1 ⊕ t2)
    (nub $ d1 ++ d2)
    (nub $ b1 ++ b2)
    (nub $ e1 ++ e2)
    (nub $ g1 ++ g2)

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
  show (DataLabel l s) = l ++ ":  " ++ s

instance Show BssLabel where
  show (BssLabel l i) = l ++ ":  resb" ++ show i

instance Show CodeFunction where
  show (CodeFunction l s) = l ++ ":\n" ++ (unlines $ map show s)

instance Show CodeBlock where
  show (LocalLabel l) = bigtabbed $ "." ++ l
  show (CodeBlob is) = unlines $ map (bigtabbed . show) is

instance Show Instruction where
  show (Add a b)    = shargs2 "add" a b
  show (Adc a b)    = shargs2 "adc" a b
  show (Sub a b)    = shargs2 "sub" a b
  show (Sbb a b)    = shargs2 "sbb" a b
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
  show (Bt a b)     = shargs2 "bt" a b
  show (Btr a b)    = shargs2 "btr" a b
  show (Bts a b)    = shargs2 "bts" a b
  show (Inc a)      = shargs1 "inc" a
  show (Dec a)      = shargs1 "dec" a
  show (Div a)      = shargs1 "div" a
  show (Mul a)      = shargs1 "mul" a
  show (Push a)     = shargs1 "push" a
  show (Pop a)      = shargs1 "pop" a
  show (Jump l)     = shargs1 "jmp" l
  show (Jcc c l)    = shargs1 c l
  show (Call l)     = shargs1 "call" l
  show (Enter a b)  = shargs2 "enter" a b
  show Clc          = "clc"
  show Pushf        = "pushf"
  show Popf         = "popf"
  show Leave        = "leave"
  show Ret          = "ret"
  show Nop          = "nop"

strip' :: String -> String
strip' = unpack . Data.Text.strip . pack

instance Show Assembler where
  show (Assembler t d b e g) = strip' (
    unlines (map ((++) "extern ") e) ++ "\n" ++
    unlines (map ((++) "global ") g) ++ "\n" ++
    "\nsection .text\n" ++ (unlines $ map show t) ++
    "\nsection .data\n" ++ (unlines $ map (bigtabbed . show) d) ++
    "\nsection .bss\n"  ++ (unlines $ map (bigtabbed . show) b))
