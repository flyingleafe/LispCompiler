{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NoImplicitPrelude #-}

module Compiler where

import Prelude
import Prelude.Unicode
import Settings
import Assembler
import SExp
import Control.Monad.State
import Control.Applicative

type Error = String
type Name = String

data Function = Function { fname  :: Name
                         , flabel :: Label
                         , args  :: [Name]
                         , fbody  :: SExp
                         }

data Builtin = Extern { name :: Name, label :: Label }
             | Inline { name :: Name, body :: [CodeBlock] }

data CompilerState = CS { functions :: [(Name, Function)] }

type Compiler = StateT CompilerState (Either Error)

-- here should be put +, -, *, / and others
builtins :: [Builtin]
builtins = [{-- TBD --}]

builtinName :: Name → Bool
builtinName nm = any (\b → name b ≡ nm) builtins

compile :: [Flag] → Program → Either Error Assembler
compile flags prog = evalStateT (compileM flags prog) (CS [])

compileM :: [Flag] → Program → Compiler Assembler
compileM flags prog = do
  buildScopeTables prog

  let defines = filter isFunDefinition prog
  functions ← mapM compileFunction defines

  let code = Assembler functions [] [] [] []

  if WithoutMain ∈ flags
  then return code
  else do
    let body = filter (not ∘ isFunDefinition) prog
    main ← compileBody $ Progn body
    let mainFun = CodeFunction "main" main
    return $ addFunction mainFun code

isFunDefinition :: SExp → Bool
isFunDefinition (Define _ (Lambda _ _)) = True
isFunDefinition _ = False

compileFunction :: Function → Compiler CodeFunction
compileFunction foo = if builtinName (fname foo)
                      then fail $ "Name '" ++ fname foo ++ " is reserved"
                      else do
                        body ← compileBody $ fbody foo
                        return $ CodeFunction (flabel foo) $ body ++ [CodeBlob [Ret]]

compileBody :: SExp → Compiler [CodeBlock]
compileBody (Progn ss) = concat <$> mapM compileBody ss
compileBody _ = (⊥)

{--
  it should build the scope tables and check everything
  TBD
--}
buildScopeTables :: Program → Compiler ()
buildScopeTables prog = return ()

{--
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
--}
