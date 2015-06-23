{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Utils where

import Prelude.Unicode
import Data.List

unionMap :: Eq a ⇒ (b → [a]) → [b] → [a]
unionMap f = nub ∘ concatMap f

enumerate :: String → [String]
enumerate pref = [pref] ++ concatMap enum' (enumerate pref)
    where enum' s = map (s ++) digits
          digits = map (:[]) ['0'..'9']

from :: Int → [Int]
from n = iterate (+1) n
