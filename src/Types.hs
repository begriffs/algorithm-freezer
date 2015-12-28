module Types (
    UnorderedPair
  , makeUnordered
  ) where

type UnorderedPair a = (a, a)

makeUnordered :: Ord a => (a,a) -> UnorderedPair a
makeUnordered (i,j) = (min i j, max i j)
