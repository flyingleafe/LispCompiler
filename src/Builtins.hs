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
             | Inline { name :: String, argn :: Int, body :: [[CodeBlock]] → [CodeBlock] }

-- True if name is reserved for builtin
builtinName :: String → Bool
builtinName nm = any (\b → name b ≡ nm) builtins

getBuiltin :: String → Maybe Builtin
getBuiltin nm = find (\b → name b ≡ nm) builtins

builtins :: [Builtin]
builtins = [ Inline "+" 2 plus
           , Inline "-" 2 minus
           , Inline "*" 2 mul
           -- , Inline "/" 2 div
           ]

plus, minus, mul :: [[CodeBlock]] → [CodeBlock]
plus [a, b] = a ⊕
              [CodeBlob [Push "rdx", Mov "rdx" "rax"]] ⊕
              b ⊕
              [CodeBlob [Add "rax" "rdx", Pop "rdx"]]

minus [a, b] = b ⊕
               [CodeBlob [Push "rdx", Mov "rdx" "rax"]] ⊕
               a ⊕
               [CodeBlob [Sub "rax" "rdx", Pop "rdx"]]

mul [a, b] = a ⊕
             [CodeBlob [Push "rdx", Mov "rdx" "rax"]] ⊕
             b ⊕
             [CodeBlob [Mul "rdx", Pop "rdx"]]

-- div [a, b] = a ++
             -- [CodeBlob [Push "rdx"]]
