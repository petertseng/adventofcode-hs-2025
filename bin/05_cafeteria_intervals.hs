import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Control.Arrow ((***))
import Data.List (sortOn)

type Interval = (Int, Int)

fresh :: [Interval] -> Int -> Bool
fresh ranges i = any (`contain` i) ranges
  where (l, r) `contain` x = l <= x && x <= r

merge :: [Interval] -> [Interval]
merge = mergeSorted . sortOn fst

mergeSorted :: [Interval] -> [Interval]
mergeSorted [] = []
mergeSorted ((min1, max1):(min2, max2):xs) | succ max1 >= min2 = mergeSorted ((min1, max max1 max2) : xs)
mergeSorted (x:xs) = x : mergeSorted xs

size :: Interval -> Int
size (l, r) = r - l + 1

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

range :: String -> (Int, Int)
range = (read *** read) . splitOnOne '-'

main :: IO ()
main = do
  s <- readInputFile
  let (ranges, ingreds) = case splitOn "" (lines s) of
        [r, i] -> (merge (map range r), map read i)
        _ -> error ("bad ranges or ingredients " ++ s)
  print (count (fresh ranges) ingreds)
  print (sum (map size ranges))
