{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO
import System.Environment
import System.Directory
import Data.List (nub)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS

import Parser.Lisp
import Compiler
import Settings
import Assembler
import LibLoader
import SExp

processIO :: ([Handle] → Handle → [Flag] → IO ()) → IO ()
processIO handling = do
  args ← parseArgs <$> getArgs
  case args of
   Left err → putStrLn $ "Can't parse arguments: " ++ err ++ "\n" ++ usage
   Right (flags', inputs) → do
     let flags = nub flags'
     inputFiles ← mapM (\x → openFile x ReadMode) inputs
     output ← openFile (outputOrDefault flags) WriteMode
     handling inputFiles output flags
     mapM hClose inputFiles
     hClose output

main :: IO ()
main = processIO $ \inputs output flags →
  do contents ← mapM BS.hGetContents inputs
     case mapM getSExps contents of
       Left err → hPutStrLn output $ "Couldn't parse: " ++ err
       Right term → do
         libs ← loadLibs flags
         putStrLn $ "number of expressions parsed: " ++ show (length term)
         case libs of
          Left err → hPutStrLn output $ "Couldn't load the library: " ++ err
          Right libs → do
            let res = case compile flags libs $ concat term of
                       Left err → "Compilation error: " ++ err
                       Right prog → show prog
            hPutStrLn output res
