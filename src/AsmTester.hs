{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Parser.Asm
import System.IO
import System.Environment
import qualified Data.ByteString.Char8 as BS


processIO :: (Handle → Handle → IO()) → IO()
processIO handling = do
  (arg1:arg2:_) ← getArgs
  input ← openFile arg1 ReadMode
  output ← openFile arg2 WriteMode
  handling input output
  hClose input
  hClose output


-- Testing source loading
main :: IO ()
main = processIO $ \input output → do
  file ← BS.hGetContents input
  putStrLn $ show $ processAssembler file
