{-# LANGUAGE UnicodeSyntax, OverloadedStrings, TupleSections #-}

module LibLoader (loadLibs) where

import Prelude.Unicode
import System.IO
import Data.List (find, nub, intersect)
import Data.Monoid
import Data.Monoid.Unicode
import Control.Monad
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS

import Settings
import Assembler
import Parser.Asm

data NamedLib = NamedLib String Assembler

isLeft :: Either a b → Bool
isLeft (Left _) = True
isLeft _ = False

{--
  Parses flags, gets all the desired libs from files if possible,
  merges them and returnes as solid assembler (or error)
--}
loadLibs :: [Flag] → IO (Either String Assembler)
loadLibs f = do
  let libPaths = getLibsNames f
  datas ← mapM openLib libPaths
  let parsed = map (\(name, code) → (name, processAssembler code)) (libPaths `zip` datas)
  if any (isLeft . snd) parsed
    then return $ Left $ "Some libraries failed to parse: " ++
         (unlines $ map show $ filter (isLeft . snd) parsed)
    else case mapM (\(a,b) → (NamedLib a) <$> b) parsed of
          Left _          → fail "FATAL ERROR" -- we've already checked parsing
          Right namedLibs → case foldM maybeMergeAsms
                                 (NamedLib "" mempty) namedLibs of
                             Left mergeErr → return $ Left $ "Failed to merge files: " ++ mergeErr
                             Right (NamedLib _ asm) → return $ Right asm

openLib :: String → IO BS.ByteString
openLib s = do
  input ← openFile s ReadMode
  content ← BS.hGetContents input
  hClose input
  return content

{--
  Returns result of lib merge or description why it can't be done
--}
maybeMergeAsms :: NamedLib → NamedLib → Either String NamedLib
maybeMergeAsms (NamedLib n1 asm1) (NamedLib n2 asm2) =
  case find (\x → fst $ x (asm1, asm2)) cantBeMergedPredicates of
   Just foo    → Left $ "for " ++ n1 ++ " and " ++ n2
                 ++ (snd $ foo (asm1, asm2))
   Nothing     → Right $ NamedLib ("Merge["++ n1 ++" & " ++ n2 ++"]")
                 $ asm1 ⊕ asm2

{--
  These are predicates for reasons when two libraries can not be merged
  together (with description)
--}
cantBeMergedPredicates :: [(Assembler, Assembler) → (Bool, String)]
cantBeMergedPredicates = [
  \(Assembler t d b _ _, _) →
   (sumlabels t d b ≢ nub (sumlabels t d b),
  "first file contains duplicate labels")
  ,
  \(_, Assembler t d b _ _) →
   (sumlabels t d b ≢ nub (sumlabels t d b),
  "second file contains duplicate labels")
  ,
  \(Assembler t d b _ _, Assembler t' d' b' _ _) →
   ((not . null) (sumlabels t d b `intersect` sumlabels t' d' b'),
   "labels in files intersect")
  ]
  where
    sumlabels :: [CodeFunction] → [DataLabel] → [BssLabel] → [String]
    sumlabels t d b = (map cflabel t) ++ (map datalabel d) ++ (map bsslabel b)
