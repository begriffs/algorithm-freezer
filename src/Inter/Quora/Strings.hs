module Inter.Quora.Strings where

import qualified Data.HashMap.Lazy as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Control.Monad.ST
import Data.Foldable (forM_)
import Data.Hashable
import Data.List (minimumBy)

import Inter.Quora.General (occurrenceGroups)

import Types

{- | Find the first non-repeated character

* Strategy

    * Make a set of elements which occur only once
    * Scan the string testing each char for membership in the set

* Complexity (let N be string length and A be alphabet size)

    * singles

        * filter: n
        * occurrenceGroups: N log A
        * N log A

    * M.toList: n
    * minimumBy: n
    * = N log A
-}
firstUniqueChar :: String -> Maybe Char
firstUniqueChar s =
  if M.null singles
     then Nothing
     else Just . fst . minimumBy occursBefore $ M.toList singles
 where
  singles = M.filter ((==1) . ogCount) $ occurrenceGroups s
  occursBefore a b = (ogFirst $ snd a) `compare` (ogFirst $ snd b)

{- | Reverse a vector recursively

Complexity: n
-}
revRecursive :: V.Vector a -> V.Vector a
revRecursive v
  | V.null v  = v
  | otherwise = V.snoc (revRecursive $ V.tail v) (V.head v)

{- | Iteratively reverse a mutable vector

* Strategy

    * Thaw the input into the ST monad
    * Pivoting around the middle, swap ends working inward
    * Freeze and return (shh, you didn't see any mutation!)

* Complexity: n
-}
revIterative :: V.Vector a -> V.Vector a
revIterative v = runST $ do
  mv <- V.thaw v
  forM_ [0..half-1] $ \i -> MV.swap mv i ((n-i)-1)
  V.freeze mv
 where
  n = V.length v
  half = n `div` 2

{- | Determine if two vectors are anagrams

* Naive n log n solution (requires Ord a)

    @
    \a b -> (V.sort a) == (V.sort b)
    @

* Strategy

    * Two strings are anagrams iff the counts of characters
      occurring in each matches those of the other
    * Build an efficient hash-based occurrence group count of each
    * Then compare

* Complexity: 3n ~ n

    * occurrenceGroups: n
    * M.map: n
    * (==) on hash maps: n
-}
anagrams :: (Eq a, Hashable a) => [a] -> [a] -> Bool
anagrams a b =
  M.map ogCount (occurrenceGroups a) ==
    M.map ogCount (occurrenceGroups b)

{- | Determine if a vector is a palindrome

* Naive three pass approach

    @
    \v -> v == reverse v
    @

* Strategy

    * It's the same complexity class really, but we can scan
      inward from both sides of the vector and check for equality
-}
palindrome :: Eq a => V.Vector a -> Bool
palindrome v = all (\i -> v V.! i == v V.! ((n-i)-1)) [0..half-1]
 where
  n = V.length v
  half = n `div` 2
