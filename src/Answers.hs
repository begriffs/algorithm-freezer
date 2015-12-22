module Answers where

import Types

import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.HashSet as H
import qualified Data.Algorithms.KMP as KMP

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
    (\k m -> M.insertWith (+) k 1 m) M.empty

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
adders n is = foldr
  (\i result ->
    if H.member (n-i) values
       then H.insert (makeUnordered (i, n-i)) result
       else result
  ) H.empty values
 where values = H.fromList is

{- | Are two lists rotations of one another?

Strategy notes:
  - The idea is to do a sublist search with modular arithmetic
  - Avoid modular arithmetic by self-concatenating one of the strings
  - Then use a standard substring search

Naive quadratic implementation
  \u v -> any (u==) [ rotate i v | i <- [0..V.length v - 1] ]
    where
      rotate i = (\(a,b) -> b V.++ a) . V.splitAt i

Time complexity:
  length: n
  KMP.build: n
  KMP.match n+m
  (++): n+m
  = n + n + 2n + n + 2n + n ~ n
-}
cycleEq :: Eq a => [a] -> [a] -> Bool
cycleEq a b = length a == length b &&
  (not . null $ KMP.match (KMP.build a) (b++b))
