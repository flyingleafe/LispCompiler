{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO
import System.Environment
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
main = do
  dc ← getCurrentDirectory
  putStrLn $ "current dir is " ++ dc
  test2 ← getSourceFromFile "./asm-sources/test2.in"
  putStrLn $
    case test2 of
     Right res → show res
     Left err  → show err
