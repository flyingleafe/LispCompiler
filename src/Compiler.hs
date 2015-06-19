{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NoImplicitPrelude #-}

module Compiler where

import Prelude
import Prelude.Unicode
import Settings
import Assembler
import SExp
import Builtins
import Control.Monad.State
import Control.Applicative hiding (Const)
import qualified Data.ByteString.Char8 as BS

type Error = String
type Name = String

{--
  This is the function definition datatype
  It contains everything to compile function into a labeled code block
  Some time after here'll be a tricky field named "closure" also
--}
data Function = Function { fname  :: Name
                         , flabel :: Label
                         , fargs  :: [Name]
                         , fbody  :: SExp
                         }

{--
  Now compiler state consists only of defined functions map,
  but a lot of other stuff will be here soon
--}
data CompilerState = CS { functions :: [(Name, Function)] }

{--
  A compiler is something with state what may fail with error,
  much like parser
--}
type Compiler = StateT CompilerState (Either Error)

compile :: [Flag] → Program → Either Error Assembler
compile flags prog = evalStateT (compileM flags prog) (CS [])

{--
  Main compile function.

  Makes 2 steps:
  1) Scope analysis and building a scope tables
  2) Making code
--}
compileM :: [Flag] → Program → Compiler Assembler
compileM flags prog = do
  buildScopeTables prog

  defines ← gets $ map snd ∘ functions
  funcs ← mapM compileFunction defines

  let flabels = map cflabel funcs
      code = Assembler funcs [] [] [] $ flabels ++ (map ("_" ++) flabels)

  if WithoutMain ∈ flags
  then return code
  else do
    let mainBody = filter (not ∘ isFunDefinition) prog
    main ← compileBody $ Progn mainBody
    let mainFun = CodeFunction "main" main
    return $ addFunction mainFun $ addGlobalLabel "main" code

isFunDefinition :: SExp → Bool
isFunDefinition (Define _ (Lambda _ _)) = True
isFunDefinition _ = False

{--
  Translates `Function` to `CodeFunction`
  Compiles function body and make it suitable assembler function body
--}
compileFunction :: Function → Compiler CodeFunction
compileFunction foo = do
  code ← compileBody $ fbody foo
  return $ CodeFunction (flabel foo) $ code ++ [CodeBlob [Ret]]

{--
  A function which should compile `SExp` into assembler code.
--}
compileBody :: SExp → Compiler [CodeBlock]
compileBody (Define _ _) = fail "Defines are not allowed in the body"
compileBody (Progn ss) = concat <$> mapM compileBody ss
compileBody (Const n) = return [CodeBlob [Mov "rax" (show n)]]
compileBody (List ((Var f):args)) =
    case getBuiltin (BS.unpack f) of
      Nothing → fail "unsupported non-builtin"
      Just b → if length args ≢ argn b
               then fail "wrong number of args"
               else body b <$> mapM compileBody args
compileBody _ = (⊥)

{--
  Performs scope analysis and scope tables building
  TBD
--}
buildScopeTables :: Program → Compiler ()
buildScopeTables prog = put $ CS $ map toFunc $ filter isFunDefinition prog
    where toFunc (Define nm (Lambda args bod)) =
              (nm', Function nm' nm' (map BS.unpack args) bod)
                  where nm' = BS.unpack nm


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
