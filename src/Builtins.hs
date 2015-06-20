{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module Builtins ( Builtin(..)
                , builtins
                , builtinName
                , getBuiltin
                ) where

import Assembler
import Prelude.Unicode
import Data.Monoid.Unicode
import Data.List

{--
  This is a datatype presenting builtin functions, like arithmetic operations.
  They are inlined.

  `name` field is name of function in Lisp,
  `argn` field is the number of arguments
  `body' determines how function body behaves around its args' bodies.
--}
data Builtin = Inline { name :: String, argn :: Int,
                        body :: [[CodeBlock]] → [CodeBlock] }

-- True if name is reserved for builtin
builtinName :: String → Bool
builtinName nm = any (\b → name b ≡ nm) builtins

-- Finds builtin with certain name and args number
getBuiltin :: String → Int → Maybe Builtin
getBuiltin nm n = find (\b → name b ≡ nm ∧ argn b ≡ n) builtins

builtins :: [Builtin]
builtins = [ Inline "not" 1 not'
           , Inline "~"   1 btw_not
           , Inline "neg" 1 neg
           , Inline "+"   1 nop
           , Inline "-"   1 neg
           , Inline "="   2 equal
           , Inline "+"   2 plus
           , Inline "-"   2 minus
           , Inline "*"   2 mul
           , Inline "/"   2 div'
           , Inline "%"   2 mod'
           ]

not', btw_not, neg, equal, plus, minus,
  mul, div', mod', nop :: [[CodeBlock]] → [CodeBlock]

not' [a] = a ⊕ [CodeBlob [Shr "rax" "1", Dec "rax", Shr "rax" "63"]]

btw_not [a] = a ⊕ [CodeBlob [Not "rax"]]

neg [a] = a ⊕ [CodeBlob [Neg "rax"]]

nop [a] = a ⊕ [CodeBlob []]

equal [a, b] = not' [a ⊕
                     [CodeBlob [Push "rax"]] ⊕
                     b ⊕
                     [CodeBlob [Pop "rdx", Sub "rax" "rdx"]]]

plus [a, b] = a ⊕
              [CodeBlob [Push "rax"]] ⊕
              b ⊕
              [CodeBlob [Pop "rdx", Add "rax" "rdx"]]

minus [a, b] = b ⊕
               [CodeBlob [Push "rax"]] ⊕
               a ⊕
               [CodeBlob [Pop "rdx", Sub "rax" "rdx"]]

mul [a, b] = a ⊕
             [CodeBlob [Push "rax"]] ⊕
             b ⊕
             [CodeBlob [Pop "rdx", Mul "rdx"]]

div' [a, b] = b ⊕
             [CodeBlob [Push "rax"]] ⊕
             a ⊕
             [CodeBlob [Pop "rcx", Xor "rdx" "rdx", Div "rcx"]]

mod' [a, b] = div' [a, b] ⊕ [CodeBlob [Mov "rax" "rdx"]]
