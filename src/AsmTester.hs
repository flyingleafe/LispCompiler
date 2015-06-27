{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Parser.Asm
import System.IO
import System.Environment
import Data.Attoparsec.ByteString.Char8(Result(..))
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
-- prints code to output, prints Result to stdout
main :: IO ()
main = processIO $ \input output → do
  file ← BS.hGetContents input
  hPutStrLn stdout $ show $ processAssemblerRes file
  case processAssembler file of
   Left err → hPutStrLn output err
   Right code → hPutStrLn output $ show code
