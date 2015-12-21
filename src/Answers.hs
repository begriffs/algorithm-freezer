module Answers where

import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M

{- | Find most frequently ocurring elts in list

Clarifying questions to ask
  - What if no elts in list?
  - What if there's a tie in frequency?

I am choosing to model the result as a set of the
the item(s) with maximal frequency.

Strategy notes:
  - Build map from items to their frequency
  - Find the maximal frequency in the map
  - Filter map for only keys with that frequency
  - Return keys as result set

Time complexity:
  countGroups: n
    lookup: log n
    insert: log n
    = n * (2 log n) ~ n log n
  maxFrequency: n
  filter: n
  keysSet: n
  = n log n + 3n ~ n log n
-}
mostFreq :: Ord a => [a] -> S.Set a
mostFreq xs =
  let grouped = countGroups xs
      maxFrequency = M.foldr max 0 grouped in
  M.keysSet $
    M.filter (==maxFrequency) grouped
 where
  countGroups :: Ord a => [a] -> M.Map a Int
  countGroups = foldr
    (\k m ->
      let old = (fromMaybe 0 $ M.lookup k m) in
      M.insert k (old + 1) m
    ) M.empty

