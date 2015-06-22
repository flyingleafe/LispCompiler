{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module LibLoader where

import Settings
import Assembler
import Data.Monoid
import Parser.Asm

{--
  Parses flags, gets all the desired libs from files if possible,
  merges them and returnes as solid assembler
--}

loadLibs :: [Flag] → Assembler
loadLibs = mempty

loadLibs' :: String → Assembler
loadLibs' s = mempty
