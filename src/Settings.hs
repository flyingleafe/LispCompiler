{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Settings where

import Prelude
import Data.List

data Flag = WithoutMain
          | SpecifiedOutput String
          deriving Eq


outputOrDefault :: [Flag] → String
outputOrDefault flags = let isSpecified (SpecifiedOutput _) = True
                            isSpecified _                   = False in
                         case find isSpecified flags of
                         Nothing                  → "a.yasm"
                         Just (SpecifiedOutput s) → s
