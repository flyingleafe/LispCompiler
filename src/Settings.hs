{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Settings where

import Prelude
import Data.List

data Flag = WithoutMain
          | DisplayHelp
          | LabelPrefixes
          | SpecifiedOutput String
          | PreprocessOnly
          | ExternLib { getExternLibPath :: String }
          deriving Eq


usage :: String
usage =
  "Usage: compiler [Flag]* InputFile \n\
  \Simple one-char flags can be combined like -abcde.\n\
  \    --help -h  show this help and exit\n\
  \    -M         disable script mode (compile without main)\n\
  \    -p         enable function labels prefix (for Darwin and Windows)\n\
  \    -S         do not compile, link with libraries only, product .yasm\n\
  \    -o output  specify output file\n\
  \    -L path    load library specified by path"

defaultFlags :: [Flag]
defaultFlags =
  [ExternLib "src/stdlib/lstdio.asm",
   ExternLib "src/stdlib/lmemory.asm",
   ExternLib "src/stdlib/lmemory_init.asm"]    -- Enable this if you're brave enough

-- Flags, input file, output files
type Configuration = ([Flag], [String])

addFlag :: Flag → Configuration → Either String Configuration
addFlag f (fs, s1) = Right (f:fs, s1)

addInput :: String → Configuration → Either String Configuration
addInput s (fs, s1) = Right (fs, s:s1)

parseArgs :: [String] → Either String Configuration
-- long flags (TBD)
parseArgs ("--help":_)     = addFlag DisplayHelp ([], [])
parseArgs (('-':'-':f):_)  = Left $ "Unknown flag: -" ++ f
-- short flags
parseArgs ("-h":_)         = addFlag DisplayHelp ([], [])
parseArgs ("-M":xs)        = parseArgs xs >>= addFlag WithoutMain
parseArgs ("-p":xs)        = parseArgs xs >>= addFlag LabelPrefixes
parseArgs ("-S":xs)        = parseArgs xs >>= addFlag PreprocessOnly
parseArgs ("-o":name:xs)   = parseArgs xs >>= addFlag (SpecifiedOutput name)
parseArgs ("-L":name:xs)   = parseArgs xs >>= addFlag (ExternLib name)
parseArgs (('-':f:[]):_)   = Left $ "Unknown flag: -" ++ [f]
parseArgs (('-':f1:fx):xs) = parseArgs (('-':[f1]):('-':fx):xs) -- multiple flags in bunch parsing
-- other stuff
parseArgs (s:[])           = Right (defaultFlags, [s])
parseArgs (s:xs)           = parseArgs xs >>= addInput s
parseArgs []               = Left $ "No input specified"

outputOrDefault :: [Flag] → String
outputOrDefault flags = let isSpecified (SpecifiedOutput _) = True
                            isSpecified _                   = False in
                         case find isSpecified flags of
                         Nothing                  → "a.yasm"
                         Just (SpecifiedOutput s) → s

getLibsNames :: [Flag] → [String]
getLibsNames f = map getExternLibPath $ filter isExternLib f
                 where isExternLib (ExternLib _) = True
                       isExternLib _             = False
