{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO
import System.Environment
import System.Directory
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS

import Parser.Lisp
import Compiler
import Settings
import Assembler
import SExp

usage :: String
usage =
  "Usage: compiler [Flag]* InputFile \n\
  \    -M         disable script mode (compile without main)\n\
  \    -p         enable function labels prefix (for Darwin and Windows)\n\
  \    -o output  specify output file"

-- Flags, input file, output files
type Configuration = ([Flag], [String])

addFlag :: Flag → Configuration → Either String Configuration
addFlag f (fs, s1) = Right (f:fs, s1)

addInput :: String → Configuration → Either String Configuration
addInput s (fs, s1) = Right (fs, s:s1)

parseArgs :: [String] → Either String Configuration
parseArgs ("-M":xs)      = parseArgs xs >>= addFlag WithoutMain
parseArgs ("-p":xs)      = parseArgs xs >>= addFlag LabelPrefixes
parseArgs ("-o":name:xs) = parseArgs xs >>= addFlag (SpecifiedOutput name)
parseArgs (('-':f):_)    = Left $ "Unknown flag: -" ++ f
parseArgs (s:[])         = Right ([], [s])
parseArgs (s:xs)         = parseArgs xs >>= addInput s
parseArgs []             = Left $ "No input specified"

processIO :: ([Handle] → Handle → [Flag] → IO ()) → IO ()
processIO handling = do
  args ← parseArgs <$> getArgs
  case args of
   Left err → putStrLn $ "Can't parse arguments: " ++ err ++ "\n" ++ usage
   Right (flags, inputs) → do
     inputFiles ← mapM (\x → openFile x ReadMode) inputs
     output ← openFile (outputOrDefault flags) WriteMode
     handling inputFiles output flags
     mapM hClose inputFiles
     hClose output

main :: IO ()
main = processIO $ \inputs output flags →
  do contents ← mapM BS.hGetContents inputs
     hPutStrLn output $ case mapM getSExps contents of
       Left err → "couldn't parse: " ++ err
       Right term → case compile flags $ concat term of
         Left err → "Compilation error: " ++ err
         Right prog → show prog
