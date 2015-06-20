{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.Directory

import Assembler
import AssemblerParser

-- Testing source loading
main :: IO ()
main = do
  dc ← getCurrentDirectory
  putStrLn $ "current dir is " ++ dc
  test2 ← getSourceFromFile "./asm-sources/test2.in"
  putStrLn $
    case test2 of
     Right res → show res
     Left err  → show err
