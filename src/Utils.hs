{-# LANGUAGE UnicodeSyntax, OverloadedStrings, OverlappingInstances, Rank2Types #-}
module Utils where

import Prelude.Unicode
import Data.List
import Control.Lens
import Control.Monad.State
import Control.Comonad
import Data.Monoid

unionMap :: Eq a ⇒ (b → [a]) → [b] → [a]
unionMap f = nub ∘ concatMap f

enumerate :: String → [String]
enumerate pref = [pref] ++ concatMap enum' (enumerate pref)
    where enum' s = map (s ++) digits
          digits = map (:[]) ['0'..'9']

exchangeBy :: (a → Bool) → a → [a] → Maybe [a]
exchangeBy f b ls = do
  i ← findIndex f ls
  return $ take i ls ++ [b] ++ drop (i + 1) ls

exchange :: Eq a ⇒ a → a → [a] → Maybe [a]
exchange a = exchangeBy (≡ a)

from :: Int → [Int]
from n = iterate (+1) n

hasEl :: (Foldable f, Eq a) ⇒ a → Getting r (f a) Bool
hasEl = to ∘ any ∘ (≡)

useAll :: MonadState s m ⇒ Getting (Endo [a]) s a → m [a]
useAll mget = do
  st ← get
  return $ st ^.. mget

infix 4 <:~, ~:>, <++~, ~\\>

(<:~) :: MonadState s m => Setting (->) s s [a] [a] -> a -> m ()
field <:~ el = field %= \l → el:l

(~:>)
  :: (Eq a, MonadState s m) => Setting (->) s s [a] [a] -> a -> m ()
field ~:> el = field %= \l → delete el l

(<++~)
  :: (Traversable t, MonadState s f) =>
     Setting (->) s s [a] [a] -> t a -> f (t ())
field <++~ els = traverse (field <:~) els

(~\\>)
  :: (Eq a, MonadState s m) =>
     Setting (->) s s [a] [a] -> [a] -> m ()
field ~\\> els = field %= \l → l \\ els

(.≟) :: MonadState s m ⇒ ASetter s s a b → Maybe b → m ()
field .≟ mval = case mval of
                  Nothing  → return ()
                  Just val → field .= val

(%≟) :: MonadState s m ⇒ Lens' s a → (a → Maybe a) → m ()
field %≟ mfoo = do
  f ← use field
  case mfoo f of
    Nothing  → return ()
    Just val → field .= val

findPacked :: (Traversable t, Comonad c, Eq a) ⇒ a → t (c a) → Maybe (c a)
findPacked x ls = foldl search Nothing ls
    where search prev next = if extract next ≡ x then Just next else prev

indexPacked :: (Comonad c, Eq a) ⇒ a → [c a] → Maybe Int
indexPacked x = findIndex (\y → extract y ≡ x)
