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
                         , frameSize :: Int
                         }

{--
  `flags` : List of passed flags
  `functions` : Map of global functions by name
  `locals` : List of current local variables. Variable value is placed
  in [ebp - N]
--}
data CompilerState = CS { flags :: [Flag]
                        , functions :: [(Name, Function)]
                        , locals :: [(Name, SExp)]
                        , sfSize :: Int
                        }

{--
  A compiler is something with state what may fail with error,
  much like parser
--}
type Compiler = StateT CompilerState (Either Error)

compile :: [Flag] → Program → Either Error Assembler
compile flags prog = evalStateT (compileM prog) (CS flags [] [] 0)

{--
  Main compile function.

  Makes 2 steps:
  1) Scope analysis and building a scope tables
  2) Making code
--}
compileM :: Program → Compiler Assembler
compileM prog = do
  buildScopeTables prog

  defines ← gets $ map snd ∘ functions
  funcs ← mapM compileFunction defines

  let flabels = map cflabel funcs
      code = Assembler funcs [] [] [] flabels

  withoutMain ← flagSet WithoutMain
  if withoutMain
  then return code
  else do
    let mainBody = filter (not ∘ isFunDefinition) prog
    main ← compileBody $ Progn mainBody
    let mainFun = CodeFunction "main" main
    return $ addFunction mainFun $ addGlobalLabel "main" code

isFunDefinition :: SExp → Bool
isFunDefinition (Define _ (Lambda _ _)) = True
isFunDefinition _ = False

flagSet :: Flag → Compiler Bool
flagSet f = gets $ (f ∈) ∘ flags

{--
  Get function label with respect to settings:
  prepend it with `_` if flag is passed
--}
getFuncLabel :: Function → Compiler Label
getFuncLabel foo = do
  isPrefixed ← flagSet LabelPrefixes
  if isPrefixed
  then return $ "_" ++ flabel foo
  else return $ flabel foo

{--
  Translates `Function` to `CodeFunction`
  Compiles function body and make it suitable assembler function body
--}
compileFunction :: Function → Compiler CodeFunction
compileFunction foo = do
  code ← compileBody $ fbody foo
  lbl ← getFuncLabel foo
  return $ CodeFunction lbl $ code ++ [CodeBlob [Ret]]

{--
  A function which should compile `SExp` into assembler code.
--}
compileBody :: SExp → Compiler [CodeBlock]
compileBody (Define _ _) = fail "Defines are not allowed in the body"
compileBody (Progn ss) = concat <$> mapM compileBody ss
compileBody (Const n) = return [CodeBlob [Mov "rax" (show n)]]
compileBody (List ((Var f):args)) =
    case getBuiltin f of
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
buildScopeTables prog = do
  state ← get
  let funcs = map toFunc $ filter isFunDefinition prog
      toFunc (Define nm (Lambda args bod)) = (nm, Function nm nm args bod sfSize)
              where sfSize = length args + countLocals bod
  put $ state { functions = funcs }

countLocals :: SExp → Int
countLocals (Let binds e) = length binds + countLocals e + (sum $ map (countLocals ∘ snd) binds)
countLocals (Cond a b c) = countLocals a + countLocals b + countLocals c
countLocals (Quote e) = countLocals e
countLocals (List ss) = sum $ map countLocals ss
countLocals (Progn ss) = sum $ map countLocals ss
