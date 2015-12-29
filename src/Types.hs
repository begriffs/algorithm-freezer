module Types (
    UnorderedPair
  , makeUnordered
  , OccurrenceGroup(..)
  ) where

type UnorderedPair a = (a, a)

makeUnordered :: Ord a => (a,a) -> UnorderedPair a
makeUnordered (i,j) = (min i j, max i j)

data OccurrenceGroup = OccurrenceGroup {
    ogFirst :: Int -- first position seen
  , ogCount :: Int -- how many seen total
  }
