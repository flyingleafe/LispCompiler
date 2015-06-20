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
  Builtin functions may be of two kinds:

  * externally linked functions, like printf (they have label to call)
  * inline operations, like arithmetic operations (they have body to inline)

  `name` field is name of function in Lisp,
  `argn` field is the number of arguments

  External builtins are called by cdecl convention.
  Inline builtins have a function which determines how it's body behave around
  its args' bodies
--}
data Builtin = Extern { name :: String, argn :: Int, label :: Label }
             | Inline { name :: String, argn :: Int, dependencies:: [Builtin],
                        body :: [[CodeBlock]] → [CodeBlock] }

-- True if name is reserved for builtin
builtinName :: String → Bool
builtinName nm = any (\b → name b ≡ nm) builtins

{--
  Added the same method with number of arguments support for function overloading
getBuiltin :: String → Maybe Builtin
getBuiltin nm = find (\b → name b ≡ nm) builtins
--}

getBuiltin :: String → Int → Maybe Builtin
getBuiltin nm n = find (\b → name b ≡ nm ∧ argn b ≡ n) builtins

builtins :: [Builtin]
builtins = [ Inline "+" 2 [] plus
           , Inline "-" 2 [] minus
           , Inline "*" 2 [] mul
           , Inline "+" 1 [] uplus
           , Inline "-" 1 [] uminus
           -- , Inline "/" 2 div
           ]

plus, minus, mul, uplus, uminus :: [[CodeBlock]] → [CodeBlock]
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

-- div [a, b] = a ++
             -- [CodeBlob [Push "rdx"]]

uplus [a] = a

uminus [a] = a ⊕
--             [CodeBlob [
--                 Mov "rdx" "rax",
--                 Xor "rax" "rax",
--                 Sub "rax" "rdx"]]
             [CodeBlob [ Neg "rax" ]]
