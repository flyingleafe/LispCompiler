{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Macros ( macroexpand
              , hasMacro ) where

import Prelude.Unicode
import Data.Maybe
import Data.List

import Utils
import SExp

data Macro = Macro { name :: String
                   , argn :: [Int]
                   , expand :: [AExp] → AExp
                   }

macros :: [Macro]
macros = [ Macro "and" (from 0) and'
         , Macro "or" (from 0) or'
         , Macro "progn" (from 0) Progn
         ] ++ carCdrShortens

carCdrShortens :: [Macro]
carCdrShortens = map ccShorten allInfixes
    where ccShorten inf = Macro ("c" ++ inf ++ "r") [1] $ ccFoo inf
          ccFoo "" [a] = a
          ccFoo ('a':ss) [a] = Funcall "car" $ [ccFoo ss [a]]
          ccFoo ('d':ss) [a] = Funcall "cdr" $ [ccFoo ss [a]]

allInfs :: Int → [String]
allInfs 0 = [""]
allInfs n = map ('a':) pr' ++ map ('d':) pr'
    where pr' = allInfs (n - 1)

allInfixes :: [String]
allInfixes = concatMap allInfs [2..4]

and', or' :: [AExp] → AExp
and' ss = foldl wrapIf (Const 1) ss
    where wrapIf a b = (Cond a b (Const 0))

or' ss = foldl wrapLetIf (Const 0) ss
    where wrapLetIf b a = (Let [("x", a)] (Cond (Var "x") (Var "x") b))

macroexpand :: String → [AExp] → AExp
macroexpand nm = expand (fromJust $ find (\m → name m ≡ nm) macros)

hasMacro :: String → Int → Bool
hasMacro nm n = any (\m → name m ≡ nm ∧ n ∈ argn m) macros
