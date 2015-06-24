{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module Builtins ( stdFuncs
                , hasBuiltin
                , applyBuiltin
                ) where

import Prelude.Unicode
import Data.Monoid.Unicode
import Data.List
import Data.Maybe

import Utils (from)
import Assembler

{--
  This is a datatype presenting builtin functions, like arithmetic operations.
  They are inlined.

  `name` field is name of function in Lisp,
  `argn` field is the number of arguments
  `body' determines how function body behaves around its args' bodies.
--}
data Builtin = Inline { name :: String, argn :: [Int],
                        body :: [[CodeBlock]] → [CodeBlock] }

-- True if name is reserved for builtin
hasBuiltin :: String → Int → Bool
hasBuiltin nm n = any (\b → name b ≡ nm ∧ n ∈ argn b) builtins

-- Finds builtin with certain name and args number
getBuiltin :: String → Int → Maybe Builtin
getBuiltin nm n = find (\b → name b ≡ nm ∧ n ∈ argn b) builtins

applyBuiltin :: String → [[CodeBlock]] → [CodeBlock]
applyBuiltin nm = body $ fromJust $ find (\b → name b ≡ nm) builtins

-- List of functions in standard library
stdFuncs :: [String]
stdFuncs = []

builtins :: [Builtin]
builtins = [ Inline "not" [1]      not'      -- this one is lognot, casts to bool
           , Inline "~"   [1]      btw_not   -- this is bitwise not
           , Inline "neg" [1]      neg       -- this is int sign negation
           , Inline "and" (from 1) and'
           , Inline "or"  (from 1) or'

           , Inline "+"   (from 1) plus
           , Inline "-"   (from 1) minus
           , Inline "*"   (from 1) mul
           , Inline "/"   (from 1) div'
           , Inline "%"   [2]      mod'

           , Inline "="   [1,2]    equal
           , Inline "<"   [2]      less
           , Inline "<="  [2]      leq
           , Inline ">"   [2]      greater
           , Inline ">="  [2]      geq
           ]

toBool, not', btw_not, neg, and', or', equal, plus, minus,
  mul, div', mod', nop, less, leq, greater, geq :: [[CodeBlock]] → [CodeBlock]

toBool [a] = not'[not'[a]]

not' [a] = a ⊕ [CodeBlob [Mov "rdx" "rax",
                          Shr "rax" "1",
                          And "rdx" "1",
                          Or "rax" "rdx",
                          Dec "rax",
                          Shr "rax" "63"]]

btw_not [a] = a ⊕ [CodeBlob [Not "rax"]]

neg [a] = a ⊕ [CodeBlob [Neg "rax"]]

and' [a]    = a
and' [a, b] = a ⊕
              [CodeBlob [Push "rax"]] ⊕
              b ⊕
              [CodeBlob [Pop "rdx",
                         And "rax" "rdx"]]
and' (x:xs) = and' $ x:[and' xs]

or' [a]    = a
or' [a, b] = a ⊕
             [CodeBlob [Push "rax"]] ⊕
             b ⊕
             [CodeBlob [Pop "rdx",
                        Or "rax" "rdx"]]
or' (x:xs) = or' $ x:[or' xs]

nop [a] = a

plus [a]    = a
plus [a, b] = a ⊕
              [CodeBlob [Push "rax"]] ⊕
              b ⊕
              [CodeBlob [Pop "rdx",
                         Add "rax" "rdx"]]
plus (x:xs) = plus $ x:[plus xs]

minus [a]    = neg [a]
minus [a, b] = b ⊕
               [CodeBlob [Push "rax"]] ⊕
               a ⊕
               [CodeBlob [Pop "rdx",
                          Sub "rax" "rdx"]]
minus (x:xs) = minus $ x:[plus xs]

mul [a]    = a
mul [a, b] = a ⊕
             [CodeBlob [Push "rax"]] ⊕
             b ⊕
             [CodeBlob [Pop "rdx",
                        Mul "rdx"]]
mul (x:xs) = mul $ x:[mul xs]

div' [a]    = a
div' [a, b] = b ⊕
             [CodeBlob [Push "rax"]] ⊕
             a ⊕
             [CodeBlob [Pop "rcx",
                        Xor "rdx" "rdx",
                        Div "rcx"]]
div' (x:xs) = div' $ x:[mul xs]

mod' [a, b] = div' [a, b] ⊕ [CodeBlob [Mov "rax" "rdx"]]

equal [a]    = toBool [a]
equal [a, b] = not' [a ⊕
                     [CodeBlob [Push "rax"]] ⊕
                     b ⊕
                     [CodeBlob [Pop "rdx",
                                Xor "rax" "rdx"]]]

less [a, b] = b ⊕
              [CodeBlob [Push "rax"]] ⊕
              a ⊕
              [CodeBlob [Pop "rdx",
                         Sub "rax" "rdx",
                         Shr "rax" "63"]]

greater [a, b] = less [b, a]

geq [a, b] = not' [less [a, b]]

leq [a, b] = not' [greater [a, b]]
