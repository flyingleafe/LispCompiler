{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NoImplicitPrelude #-}

module Compiler where

import Prelude
import Settings
import Assembler
import SExp

-- returns labels that are needed and code block
compileSexp :: SExp → ([DataLabel], [CodeBlock])
compileSexp (Const i)                       = ([], [CodeBlob [
                                                       Mov "eax" (show i),
                                                       Ret ]])
compileSexp (List ((Var "-"):(Const i):[])) = undefined

genCallingMain :: [String] → CodeFunction
genCallingMain names = CodeFunction "main" $ [CodeBlob ((map Call names) ++ [Mov "rax" "0", Ret])]

funcNames :: [String]
funcNames = map (("function" ++) . show) (iterate (+1) 0)

isDefine :: SExp → Bool
isDefine (Define _ _) = True
isDefine _            = False

compileUnnamed :: Program → ([DataLabel], [CodeFunction])
compileUnnamed sexps = let compiled = map compileSexp sexps in
                         (concatMap fst compiled, zipWith CodeFunction funcNames (map snd compiled))

compileDefines :: Program → ([DataLabel], [CodeFunction])
compileDefines s = ([], [])

compile :: [Flag] → Program → Assembler
compile flags sexps = let (datalabelsND, functionsND)
                            = compileUnnamed $ filter (not . isDefine) sexps in
                      let (datalabelsD, functionsD)
                            = compileDefines $ filter isDefine sexps in
                      Assembler [TextSec $
                                 functionsND ++
                                 functionsD ++
                                 if WithoutMain `elem` flags
                                 then []
                                 else [genCallingMain (map cflabel functionsND)]
                                ,
                                 DataSec $ datalabelsND ++ datalabelsD]
                      []
                      ["main"]
