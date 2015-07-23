{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Prelude.Unicode
import System.IO
import System.IO.Temp
import System.Environment
import System.Process
import Data.List (nub)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS

import Parser.Lisp
import Compiler
import Settings
import LibLoader
import Assembler

processIO :: ([Handle] → Handle → [Flag] → IO ()) → IO ()
processIO handling = do
  args ← parseArgs <$> getArgs
  case args of
   Left err → hPutStrLn stderr $ "Can't parse arguments: " ++ err ++ "\n" ++ usage
   Right (flags', inputs) → do
     let flags = nub flags'
     if DisplayHelp ∈ flags
     then hPutStrLn stdout usage
     else do inputFiles ← mapM (\x → openFile x ReadMode) inputs
             output ← openFile (outputOrDefault flags) WriteMode
             handling inputFiles output flags
             mapM hClose inputFiles
             hClose output

main :: IO ()
main = processIO $ \inputs output flags →
  do contents ← mapM BS.hGetContents inputs
     case mapM getSExps contents of
       Left err   → hPutStrLn stderr $ "Couldn't parse: " ++ err
       Right term → do
         libs ← loadLibs flags
         case libs of
          Left err   → hPutStrLn stderr $ "Couldn't load the library: " ++ err
          Right libs → case compile flags libs $ concat term of
                        Left err   → hPutStrLn stderr $ "Compilation error: " ++ err
                        Right prog →
                          let prog' = if LabelPrefixes ∈ flags
                                      then addLabelPrefixes prog
                                      else prog
                              yflags = if LabelPrefixes ∈ flags
                                       then ["-fmacho64"]
                                       else ["-felf64", "-gdwarf2"]
                          in
                          if PreprocessOnly ∈ flags then
                            hPutStrLn output $ show prog'
                          else do
                            (pathin, h1)  ← openTempFile "/tmp" "lispcomptemp.yasm"
                            (pathout, h2)  ← openTempFile "/tmp" "lispcomptemp.o"
                            hPutStrLn h1 $! show prog'
                            hFlush h1
                            callProcess "yasm" $ yflags ++ ["-o", pathout, pathin]
                            putStrLn "yasm suceeded"
                            hFlush h2
                            callProcess "gcc" ["-ggdb",
                                               "-o", outputOrDefault flags,
                                               pathout
                                              ]
                            putStrLn "cc suceeded"
                            hClose h1
                            hClose h2
