import AdventOfCode (readInputFile)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (elemIndices, mapAccumL, uncons)

-- can consider just a [Int] for the row,
-- because we can just go left-to-right, but the code would be more verbose
step :: IntMap Int -> IntSet -> (IntMap Int, Int)
step timelines row = (IntMap.unionWith (+) notSplit newFromSplit, IntMap.size split)
  where (split, notSplit) = IntMap.partitionWithKey (\k _ -> k `IntSet.member` row) timelines
        newFromSplit = IntMap.fromAscListWith (+) (concatMap mkSplit (IntMap.assocs split))
        mkSplit (x, n) = [(x - 1, n), (x + 1, n)]

one :: [a] -> a
one [x] = x
one [] = error "none"
one (_:_) = error "too many"

main :: IO ()
main = do
  s <- readInputFile
  let (start, splitters) = case uncons (lines s) of
        Just (x, xs) -> (one (elemIndices 'S' x), map (IntSet.fromAscList . elemIndices '^') xs)
        Nothing -> (undefined, [])
      (timelines, splits) = mapAccumL step (IntMap.singleton start 1) splitters
  print (sum splits)
  print (sum (IntMap.elems timelines))
