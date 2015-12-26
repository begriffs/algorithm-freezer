module Inter.Quora.General where

import Types

import Data.Char (digitToInt)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.HashSet as H
import qualified Data.Algorithms.KMP as KMP
import qualified Data.Vector as V

{- | Find most frequently occuring elts in list

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
  occurrenceGroups: n log n
  maxFrequency: n
  filter: n
  keysSet: n
  = n log n + 3n ~ n log n
-}
mostFreq :: Ord a => [a] -> S.Set a
mostFreq xs =
  let grouped = occurrenceGroups xs
      maxFrequency = M.foldr max 0 grouped in
  M.keysSet $
    M.filter (==maxFrequency) grouped

{- | Helper for array frequency questions

Complexity: n log n
  insertWith: log n
-}
occurrenceGroups :: Ord a => [a] -> M.Map a Int
occurrenceGroups = foldr
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

{- | Find the only element in an array which occurs only once.

Clarifying questions
  - What if there is none? More than one?

I am choosing to model the result as a set of the
the item(s) which occur exactly once.

Strategy notes:
  - Similar to finding the most frequent item
  - Build map from items to their frequency
  - Filter map for keys with one occurrence
  - Return keys as result set

Complexity:
  occurrenceGroups: n log n
  maxFrequency: n
  filter: n
  keysSet: n
  = n log n + 2n ~ n log n
-}
loners :: Ord a => [a] -> S.Set a
loners = M.keysSet . M.filter (==1) . occurrenceGroups

{- | Find the common elements of two lists

Strategy
  - The standard library implementation dodges the question
  - I'll do it properly when writing the data structures by hand

Complexity:
  S.fromList: n log n
  S.intersection: m+n
  = n log n + m + n + m log m ~ n log n (for n similar to m)
-}
commonElts :: Ord a => [a] -> [a] -> S.Set a
commonElts a b = S.fromList a `S.intersection` S.fromList b

{- | Binary search in a sorted array

Assumption
  - Input sorted in ascending order

Strategy
  - Divide and conquer with indices

Complexity
  log n
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

Assumption
  - Input was sorted in ascending order then rotated

Strategy
  - Generalization of binary search since sorted list is trivial rotation
  - At any pivot either the left or the right side is sorted (proof?)
  - If the left (right) is sorted and needle lies in its bounds then if the
    needle exists at all it must be on that side
  - Else if the needle exists it must be in the other side

Complexity
  log n
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

Strategy:
  - Use lazy evaluation to sieve a list as you go
  - [2,3,4,5,6,7,8,9,10...]
  - Yield 2 unconditionally
  - Now scan the list for a non-multiple of 2. Finds 3.
  - Scan the new list for a non-multiple of 3.
    Scanning the list causes it to be computed, and already omits 4.
    Finds 5.

Complexity
  - No idea. There are rougly n / log n primes smaller than n,
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

It's more efficient use (take n) . primesTo . (\n -> n * (logBase 2 n))
although not guaranteed to produce exactly n results.
-}
primes :: [Int]
primes = removeHeadMultiples [2..]
 where
  removeHeadMultiples [] = []
  removeHeadMultiples (p:xs) = p : removeHeadMultiples [x | x <- xs, x `rem` p > 0]

{- | Find n-ary representation of an int.

Clarifying questions
  - Should it handle negative numbers too?

Complexity:
  decompose: log n
  reverse: n
  = 2 log n ~ log n
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

Clarifying questions
  - Should it handle negative numbers too?

Complexity:
  zipWith: n
  reverse: n
  map: n
    digitToInt: 1
    = n
  = 3n ~ n
-}
parseInt :: String -> Int
parseInt ('-':str) = -1 * parseInt str
parseInt str =
  sum . zipWith (*) [10^i | i::Int <- [0..]]
    . reverse
    $ map digitToInt str

{- | Estimate the square root of a number to a tolerance

Strategy
  - Search for sqrt in a region, dividing region size in half each time

Complexity:
  - This searches through up to (n / sqrt tolerance) intervals
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

Clarifying questions:
  - Nonnegative exponents only?

Strategy:
  - Multiplication is associative so x^(2i+r) = ((x^i)^2)(x^r)
    which allows us to divide and conquer: i is half the original
    exponent and r is at most one.

Naive O(n) way:
  \base, n -> product . take n $ repeat base
-}
raiseTo :: Integer -> Integer -> Integer
raiseTo _    0 = 1
raiseTo base 1 = base
raiseTo base n =
  let halfsies = raiseTo base (n `quot` 2)
      extra    = raiseTo base (n `rem` 2) in
  halfsies * halfsies * extra

{- | Multiplication defined in terms of addition

Clarifying questions:
  - The two arguments are natural numbers? Integers? Rationals? Reals? Complex?
  - Assuming integers

Naive way to multiply positives:
  sum . take b $ repeat a

Complexity:
  log (min a b)
-}
multiplyBy :: Integer -> Integer -> Integer
multiplyBy a b =
  let {a' = abs a; b' = abs b} in
  (if xorNegative a b then negate else id)
    (multPos (max a' b') (min a' b'))
 where
  xorNegative m n =
    let { posm = m < 0; posn = n < 0 } in
    (posm || posn) && not (posm && posn)
  multPos _ 0 = 0
  multPos m 1 = m
  multPos m n =
    let halfsies = multPos m (n `quot` 2)
        extra    = multPos m (n `rem` 2) in
    halfsies + halfsies + extra
