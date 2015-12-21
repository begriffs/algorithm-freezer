module Answers where

import Types

import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.HashSet as H

{- | Find most frequently ocurring elts in list

Clarifying questions
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

{- | Find pairs of integers in a list which add to n.

Clarifying assumptions
  - No duplicate pairs allowed
  - Output canonical pairs, i.e. (a,b) for a <= b

Strategy notes:
  - Populate a hash set of values present in the array
  - Traverse each and search for its additive complement
  - If found add unordered pair to result set

Naive quadratic implementation with ordered pairs
  \ns -> [(a,b) | a <- ns, b <- ns, a+b==n]

Time complexity: for arbitrary list length but bounded W
  H.fromList: n*min(10, W) ~ n (for small W)
  H.member: min(10, W) ~ 1
  H.insert: min(10, W) ~ 1
  = n + n*(1 + 1 + 1) ~ n
-}
adders :: Int -> [Int] -> H.HashSet (UnorderedPair Int)
adders n is =
  let values = H.fromList is in
  foldr
    (\i result ->
      if H.member (n-i) values
         then H.insert (makeUnordered (i, n-i)) result
         else result
    ) H.empty values
