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
         ]

and', or' :: [AExp] → AExp
and' ss = foldl wrapIf (Const 1) ss
    where wrapIf a b = (Cond a b (Const 0))

or' ss = foldl wrapLetIf (Const 0) ss
    where wrapLetIf b a = (Let [("x", a)] (Cond (Var "x") (Var "x") b))

macroexpand :: String → [AExp] → AExp
macroexpand nm = expand (fromJust $ find (\m → name m ≡ nm) macros)

hasMacro :: String → Int → Bool
hasMacro nm n = any (\m → name m ≡ nm ∧ n ∈ argn m) macros
