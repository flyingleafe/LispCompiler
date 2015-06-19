{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Compiler where

import Prelude
import Assembler
import SExp

-- returns labels that are needed and code block
compileSexp :: SExp → ([DataLabel], [CodeBlock])
compileSexp (List (o@(Const _):[])) = compileSexp o
compileSexp (Const i)               = ([], [CodeBlob [
                                               Mov "rax" (show i),
                                               Ret ]])

genCallingMain :: [String] → CodeFunction
genCallingMain names = CodeFunction "main" $ [CodeBlob ((map Call names) ++ [Ret])]

funcNames :: [String]
funcNames = map (("func" ++) . show) (iterate (+1) 0)

isDefine :: SExp → Bool
isDefine (Define _ _) = True
isDefine _            = False

compile :: Program → Assembler
compile sexps = let exprs = map compileSexp $ filter (not . isDefine) sexps in
                 let functions = zipWith CodeFunction funcNames (map snd exprs) in
                  Assembler [TextSec $
                             genCallingMain (map cflabel functions) : functions ,
                             DataSec $ concat $ map fst exprs]
                  []
                  ["main"]
