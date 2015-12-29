module Inter.Quora.Strings where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (find)

import Inter.Quora.General (occurrenceGroups)

{- | Find the first non-repeated character

Strategy
  - Make a set of elements which occur only once
  - Scan the string testing each char for membership in the set

Complexity
  - find: n
  - singles
    - fromList: n log n
    - keys: n
    - filter: n
    - occurrences: n log n
    = 2 (n + n log n) ~ n log n
  - member: log n
  = n + n log n + log n ~ n log n
-}
firstUniqueChar :: String -> Maybe Char
firstUniqueChar s = find (`Set.member` singles) s
 where
  occurrences = occurrenceGroups s
  singles = Set.fromList . Map.keys $ Map.filter (==1) occurrences
