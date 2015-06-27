{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO
import System.Environment
import Data.List (nub)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS

import Parser.Lisp
import Compiler
import Settings
import LibLoader

processIO :: ([Handle] → Handle → [Flag] → IO ()) → IO ()
processIO handling = do
  args ← parseArgs <$> getArgs
  case args of
   Left err → hPutStrLn stderr $ "Can't parse arguments: " ++ err ++ "\n" ++ usage
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
       Left err → hPutStrLn stderr $ "Couldn't parse: " ++ err
       Right term → do
         libs ← loadLibs flags
         case libs of
          Left err → hPutStrLn stderr $ "Couldn't load the library: " ++ err
          Right libs → case compile flags libs $ concat term of
                        Left err → hPutStrLn stderr $ "Compilation error: " ++ err
                        Right prog → hPutStrLn output $ show prog
