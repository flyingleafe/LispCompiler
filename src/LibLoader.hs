{-# LANGUAGE UnicodeSyntax, OverloadedStrings, TupleSections #-}

module LibLoader (NamedLib(..),
                  loadLibs, mergeNamedLibs, libExterns) where

import System.IO
import System.Directory
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS

import Settings
import Assembler
import Parser.Asm

data NamedLib = NamedLib { getLibName :: String, getLibCode :: Assembler }

isLeft :: Either a b → Bool
isLeft (Left _) = True
isLeft _ = False

libExterns :: NamedLib → [String]
--libExterns = (map cflabel) . textSec . getLibCode
libExterns = globalLabels . getLibCode

{--
  Parses flags, gets all the desired libs from files if possible.
--}
loadLibs :: [Flag] → IO (Either String [NamedLib])
loadLibs f = do
  let libPaths = getLibsNames f
  datas ← mapM openLib libPaths
  let parsed = map (\(name, code) → (name, processAssembler code)) (libPaths `zip` datas)
  if any (isLeft . snd) parsed
    then return $ Left $ "Some libraries failed to parse: " ++
         (unlines $ map show $ filter (isLeft . snd) parsed)
    else case mapM (\(a,b) → (NamedLib a) <$> b) parsed of
          Left _          → fail "FATAL ERROR" -- we've already checked parsing
          Right namedLibs → return $ Right namedLibs

{--
  Merges them and returnes as solid assembler (or error)
--}
mergeNamedLibs :: [NamedLib] → Either String Assembler
mergeNamedLibs namedLibs = case foldl
                           (\(a1, a2, asm) (NamedLib b asm') →
                             ("Merge[" ++ a1 ++ ", " ++ a2 ++ "]",
                              b, (maybeMergeAsms asm') =<< asm))
                           ("", "", Right $ Assembler [] [] [] [] [])
                           namedLibs of
                       (a1, a2, Left mergeErr) →
                         Left $ "Failed to merge files " ++ a1 ++
                         " " ++ a2 ++ ": " ++ mergeErr
                       (_, _, o@(Right _)) → o

openLib :: String → IO BS.ByteString
openLib s = do
  exist ← doesFileExist s
  if not exist then putStrLn $ s ++ " does not exist" else return ()
  input ← openFile s ReadMode
  content ← BS.hGetContents input
  hClose input
  return content
