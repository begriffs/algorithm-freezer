module Inter.Quora.Strings where

import qualified Data.HashMap.Lazy as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Control.Monad.ST
import Data.Foldable (forM_)
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
revIterative v
  | V.null v  = v
  | otherwise = runST $ do
    mv <- V.thaw v
    forM_ [0..half-1] $ \i -> MV.swap mv i ((n-i)-1)
    V.freeze mv
 where
  n = V.length v
  half = n `div` 2
