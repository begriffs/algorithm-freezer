module Inter.Quora.Strings where

import qualified Data.HashMap.Lazy as M
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
