{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO
import System.Environment
import Text.ParserCombinators.Parsec
import System.Directory

import Parser
import Assembler
import SExp

processIO :: (Handle → Handle → IO()) → IO()
processIO handling = do
  (arg1:arg2:_) ← getArgs
  dc ← getCurrentDirectory
  putStrLn dc
  input ← openFile arg1 ReadMode
  output ← openFile arg2 WriteMode
  handling input output
  hClose input
  hClose output


main :: IO ()
--main = (⊥)
main = processIO $ \input output → do
  s ← hGetContents input
--  hPutStrLn output $ case parse (parseCodeBlocks >> getInput) "mda" s of
  hPutStrLn output $ case parseTextSec s of
   Right res → show res
   Left err  → show err
