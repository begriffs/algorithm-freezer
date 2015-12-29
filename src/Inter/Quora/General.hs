module Inter.Quora.General where

import Types

import Control.Monad (replicateM)
import Data.Char (digitToInt)
import Data.Graph.Inductive hiding (edges, nodes, neighbors, mkNode, mkEdge)
import Data.Hashable
import Data.Maybe (catMaybes)
import Data.Matrix

import qualified Data.Algorithms.KMP as KMP
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Vector as V

import Data.Random

{- | Find most frequently occuring elts in list

* Clarifying questions

    * What if no elts in list?
    * What if there's a tie in frequency?

I am choosing to model the result as a set of the
the item(s) with maximal frequency.

* Strategy notes

    * Build map from items to their frequency
    * Find the maximal frequency in the map
    * Filter map for only keys with that frequency
    * Return keys as result set

* Time complexity:

    * occurrenceGroups: n log n
    * maxFrequency: n
    * filter: n
    * keys: n
    * fromList: O(n*min(W, n)) ~ n
    * = n log n + 4n ~ n log n
-}
mostFreq :: (Eq a, Hashable a) => [a] -> S.HashSet a
mostFreq xs =
  let grouped = occurrenceGroups xs
      maxFrequency = M.foldr (max . ogCount) 0 grouped in
  S.fromList . M.keys $
    M.filter ((==maxFrequency) . ogCount) grouped

{- | Helper for array frequency questions

* Complexity: n log n

    * insertWith: log n
-}
occurrenceGroups :: (Eq a, Hashable a) => [a] -> M.HashMap a OccurrenceGroup
occurrenceGroups = foldr (\(pos,k) m ->
    M.insertWith (\og _ -> og { ogCount = ogCount og + 1 })
      k (OccurrenceGroup pos 1) m
  ) M.empty . zip [0..]

{- | Find pairs of integers in a list which add to n.

* Clarifying assumptions

    * No duplicate pairs allowed
    * Output canonical pairs, i.e. (a,b) for a <= b

* Strategy notes:

    * Populate a hash set of values present in the array
    * Traverse each and search for its additive complement
    * If found add unordered pair to result set

* Naive quadratic implementation with ordered pairs

    > \ns -> [(a,b) | a <- ns, b <- ns, a+b==n]

* Time complexity: for arbitrary list length but bounded W

    * M.fromList: n*min(10, W) ~ n
    * M.member: min(10, W) ~ 1
    * M.insert: min(10, W) ~ 1
    * = n + n*(1 + 1 + 1) ~ n
-}
adders :: Int -> [Int] -> S.HashSet (UnorderedPair Int)
adders n is = foldr
  (\i result ->
    if S.member (n-i) values
       then S.insert (makeUnordered (i, n-i)) result
       else result
  ) S.empty values
 where values = S.fromList is

{- | Are two lists rotations of one another?

* Strategy notes

    * The idea is to do a sublist search with modular arithmetic
    * Avoid modular arithmetic by self-concatenating one of the strings
    * Then use a standard substring search

* Naive quadratic implementation

    >\u v -> any (u==) [ rotate i v | i <- [0..V.length v - 1] ]
    >  where
    >    rotate i = (\(a,b) -> b V.++ a) . V.splitAt i

* Time complexity

    * length: n
    * KMP.build: n
    * KMP.match n+m
    * (++): n+m
    * = n + n + 2n + n + 2n + n ~ n
-}
cycleEq :: Eq a => [a] -> [a] -> Bool
cycleEq a b = length a == length b &&
  (not . null $ KMP.match (KMP.build a) (b++b))

{- | Find the only element in an array which occurs only once.

* Clarifying questions

    * What if there is none? More than one?

I am choosing to model the result as a set of the
the item(s) which occur exactly once.

* Strategy notes
    * Similar to finding the most frequent item
    * Build map from items to their frequency
    * Filter map for keys with one occurrence
    * Return keys as result set

* Complexity

    * occurrenceGroups: n log n
    * maxFrequency: n
    * filter: n
    * keysSet: n
    * = n log n + 2n ~ n log n
-}
loners :: (Eq a, Hashable a) => [a] -> S.HashSet a
loners = S.fromList . M.keys . M.filter ((==1) . ogCount) . occurrenceGroups

{- | Find the common elements of two lists

* Strategy

    * The standard library implementation dodges the question
    * I'll do it properly when writing the data structures by hand

* Complexity

    * S.fromList: n log n
    * S.intersection: m+n
    * = n log n + m + n + m log m ~ n log n (for n similar to m)
-}
commonElts :: (Eq a, Hashable a) => [a] -> [a] -> S.HashSet a
commonElts a b = S.fromList a `S.intersection` S.fromList b

{- | Binary search in a sorted array

* Assumption

    * Input sorted in ascending order

* Strategy

    * Divide and conquer with indices

* Complexity

    * log n
-}
searchSorted :: Ord a => a -> V.Vector a -> Maybe Int
searchSorted a ar =
  inRange 0 (V.length ar - 1)
 where
  inRange lo hi
    | lo > hi = Nothing
    | a == guess = Just mid
    | a > guess = inRange (mid+1) hi
    | otherwise = inRange lo (mid-1)
    where
      mid = (hi+lo) `div` 2
      guess = ar V.! mid

{- | Binary search in a sorted-then-rotated array

* Assumption

    * Input was sorted in ascending order then rotated

* Strategy

    * Generalization of binary search since sorted list is trivial rotation
    * At any pivot either the left or the right side is sorted (proof?)
    * If the left (right) is sorted and needle lies in its bounds then if the
      needle exists at all it must be on that side
    * Else if the needle exists it must be in the other side

* Complexity

    * log n
-}
searchRotated :: Ord a => a -> V.Vector a -> Maybe Int
searchRotated a ar =
  inRange 0 (V.length ar - 1)
 where
  inRange lo hi
    | lo > hi = Nothing
    | a == mdval = Just mid
    | losorted && loval <= a && a < mdval = inRange lo (mid-1)
    | hisorted && mdval < a && a <= hival = inRange (mid+1) hi
    | not losorted = inRange lo (mid-1)
    | not hisorted = inRange (mid+1) hi
    | otherwise = Nothing  --both sides sorted but elt not there
    where
      mid = (hi+lo) `div` 2
      loval = ar V.! lo
      mdval = ar V.! mid
      hival = ar V.! hi
      losorted = loval <= mdval
      hisorted = mdval < hival

{- | Find primes less than n

Could this solution be considered "dynamic programming?" Does
the lazy evaluation constitute a lookup table?

* Strategy

    * Use lazy evaluation to sieve a list as you go
    * [2,3,4,5,6,7,8,9,10...]
    * Yield 2 unconditionally
    * Now scan the list for a non-multiple of 2. Finds 3.
    * Scan the new list for a non-multiple of 3.
      Scanning the list causes it to be computed, and already omits 4.
      Finds 5.

* Complexity

    * No idea. There are rougly n / log n primes smaller than n,
      and continuing to traverse a more deeply sieved list layers on
      divisibility checks, it has to be worse than linear time.
-}
primesTo :: Int -> [Int]
primesTo n = removeHeadMultiples [2..n]
 where
  removeHeadMultiples [] = []
  removeHeadMultiples same@(p:xs)
    | p*p > n   = same -- short circuit factors greater than sqrt(n)
    | otherwise = p : removeHeadMultiples [x | x <- xs, x `rem` p > 0]

{- | List of all primes, take the first n items as desired

Like primesTo but less efficient because it ends up sieving the
lists for terms which fall outside the chosen range.

It's more efficient to use

> (take n) . primesTo . (\n -> n * (logBase 2 n))

although not guaranteed to produce exactly n results.
-}
primes :: [Int]
primes = removeHeadMultiples [2..]
 where
  removeHeadMultiples [] = []
  removeHeadMultiples (p:xs) = p : removeHeadMultiples [x | x <- xs, x `rem` p > 0]

{- | Find n-ary representation of an int.

* Clarifying questions

    * Should it handle negative numbers too?

* Complexity:

    * decompose: log n
    * reverse: n
    * = 2 log n ~ log n
-}
naryRepresentation :: Int -> Int -> [Int]
naryRepresentation base n =
  reverse $ decompose n
 where
  decompose i
    | i < 0 = error "Cannot handle negative numbers"
    | i < base = [i `rem` base]
    | otherwise = (i `rem` base) : decompose (i `div` base)

{- | Turn string of numerals into an integer

* Clarifying questions

    * Should it handle negative numbers too?

* Complexity

    * zipWith: n
    * reverse: n
    * map: n
        * digitToInt: 1
        * = n
    * = 3n ~ n
-}
parseInt :: String -> Int
parseInt ('-':str) = -1 * parseInt str
parseInt str =
  sum . zipWith (*) [10^i | i::Int <- [0..]]
    . reverse
    $ map digitToInt str

{- | Estimate the square root of a number to a tolerance

* Strategy

    * Search for sqrt in a region, dividing region size in half each time

* Complexity

    * This searches through up to (n / sqrt tolerance) intervals
      to find the value. Because it eliminates half the search
      space each time I think it runs in log (n / sqrt tolerance)
-}
findRoot :: Double -> Double -> Double
findRoot tolerance n
  | n < 0 = error "Negative numbers outside of domain"
  | n < 1 = rootIn n 1 -- the root will be bigger than n
  | otherwise = rootIn 0 n
 where
  rootIn lo hi =
    let mid = (lo + hi) / 2 in
    if abs (n - mid*mid) < tolerance
      then mid
      else
        if mid*mid < n then rootIn mid hi else rootIn lo mid

{- | Exponentiation defined in terms of multiplication

* Clarifying questions

    * Nonnegative exponents only?

* Strategy

    * Multiplication is associative so x^(2i+r) = ((x^i)^2)(x^r)
      which allows us to divide and conquer: i is half the original
      exponent and r is at most one.

Naive O(n) way:

    > \base, n -> product . take n $ repeat base
-}
raiseTo :: Integer -> Integer -> Integer
raiseTo _    0 = 1
raiseTo base 1 = base
raiseTo base n =
  let halfsies = raiseTo base (n `quot` 2)
      extra    = raiseTo base (n `rem` 2) in
  halfsies * halfsies * extra

{- | Multiplication defined in terms of addition

* Clarifying questions

    * The two arguments are natural numbers? Integers? Rationals? Reals? Complex?
    * Assuming integers

Naive way to multiply positives:

    > sum . take b $ repeat a


* Complexity:
    * log (min a b)
-}
multiplyBy :: Integer -> Integer -> Integer
multiplyBy a b =
  let {a' = abs a; b' = abs b} in
  (if xorNegative a b then negate else id)
    (multPos (max a' b') (min a' b'))
 where
  xorNegative m n = (negm || negn) && not (negm && negn)
    where { negm = m < 0; negn = n < 0 }
  multPos _ 0 = 0
  multPos m 1 = m
  multPos m n =
    let halfsies = multPos m (n `quot` 2)
        extra    = multPos m (n `rem` 2) in
    halfsies + halfsies + extra

{- | Generate random numbers from 0-6 given generator for 0-4

* Strategy

    * over-roll the d5 to generate more than 7 possibilities
    * map certain ranges of those possibilities to the desired 0-6
    * consider the remaining possibilities as a misroll and try again
    * it's a "Las Vegas" algorithm and overwhelmingly likely to terminate
-}
d7 :: RVar Int
d7 = do
  [x, y] <- replicateM 2 $ uniform 0 (4::Int)
  let d9 = (5*x + y) `quot` 3 -- 25 `quod` 3 == 8 > 6
  if d9 > 6 then d7 else return d9

{- | Given a matrix of booleans count the number of islands of Trues

* Clarifying questions

    * Do diagonal neighbors count for connectedness? (Assuming yes)

* Strategy

    * True cells in the matrix are land, False are sea
    * This is a disguised instance of finding the connected components
      in a graph
    * Transform the matrix into a proper graph of land nodes
    * For each land node add connections for each of its land
      neighbors in the matrix
    * (This will lead to double marking the connections forward and
       backward but does not change the time complexity)
    * Then find and count the connected components

* Complexity for m x n matrix

    * nodes: mn
    * neighbors: 1
    * components (via DFS): linear in number of land nodes
    * = mn
-}
countIslands :: Matrix Bool -> Int
countIslands = length . components . landGraph
 where
  landGraph :: Matrix Bool -> Gr CellLabel ()
  landGraph m = mkGraph land edges
   where
    land = landOnly $ nodes m
    edges = [mkEdge n1 n2 |
             n1 <- land, n2 <- landOnly $ neighbors n1 m]

  landOnly :: [LNode CellLabel] -> [LNode CellLabel]
  landOnly = filter (clLand . snd)

  nodes :: Matrix Bool -> [LNode CellLabel]
  nodes m = catMaybes
    [ mkNode (i,j) m | i <- [1..nrows m], j <- [1..ncols m] ]

  neighbors :: LNode CellLabel -> Matrix Bool -> [LNode CellLabel]
  neighbors (_, CellLabel (i,j) _) m = catMaybes [ mkNode (a,b) m |
    a <- [i-1,i,i+1], b <- [j-1,j,j+1], not (a==i && b==j) ]

  mkNode :: (Int,Int) -> Matrix Bool -> Maybe (LNode CellLabel)
  mkNode (i,j) m = (i * ncols m + j,) <$> (CellLabel (i,j) <$> safeGet i j m)

  mkEdge :: LNode CellLabel -> LNode CellLabel -> UEdge
  mkEdge n1 n2 = (fst n1, fst n2, ())

data CellLabel = CellLabel {
    clPos  :: (Int,Int)  -- The cell's position in its matrix
  , clLand :: Bool       -- True for land, False for sea
  } deriving (Eq)
