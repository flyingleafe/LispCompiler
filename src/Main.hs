{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO
import System.Environment
import System.Directory
import qualified Data.ByteString.Char8 as BS

import Parser
import Compiler
import Assembler
import SExp

processIO :: (Handle → Handle → IO()) → IO()
processIO handling = do
  (arg1:arg2:_) ← getArgs
  dc ← getCurrentDirectory
  System.IO.putStrLn dc
  input ← openFile arg1 ReadMode
  output ← openFile arg2 WriteMode
  handling input output
  hClose input
  hClose output


main :: IO ()
--main = (⊥)

main = processIO $ \input output →
  do s ← BS.hGetContents input
     hPutStrLn output $ case getSExp s of
       Left error → "couldn't parse: " ++ error
       Right term → show $ compile [term]
