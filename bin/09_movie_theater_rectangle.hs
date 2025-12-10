{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Data.Array.IArray ((!), Array, accumArray)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (rights)
import qualified Data.IntMap as IntMap
import Data.List (sort, tails)

type Pos = (Int, Int)

line :: Pos -> Pos -> [Pos]
line (y1, x1) (y2, x2)
  | y1 == y2 = map (y1, ) [min x1 x2 .. max x1 x2]
  | x1 == x2 = map (, x1) [min y1 y2 .. max y1 y2]
  | otherwise = error ("bad line" ++ show ((x1, y1), (x2, y2)))

main :: IO ()
main = do
  s <- readInputFile
  let pts = map ((read *** read) . splitOnOne ',') (lines s)
      pairs = [(x, y) | x:ys <- tails pts, y <- ys]
      size ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
      sizes = map size pairs
  print (maximum sizes)
  let ys = sort (nubOrd (map snd pts))
      xs = sort (nubOrd (map fst pts))
      idy = IntMap.fromAscList (zip ys [1..])
      idx = IntMap.fromAscList (zip xs [1..])
      -- Note: now (y, x)
      pts' = map (\(x, y) -> (idy IntMap.! y, idx IntMap.! x)) pts
      outline = concat (zipWith line pts' (drop 1 (cycle pts')))
      ymax = length ys
      xmax = length xs
      green :: Array Pos Bool
      green = accumArray (||) False ((1, 1), (ymax, xmax)) (map (, True) outline)
      inBounds (y, x) = 1 <= y && y <= ymax && 1 <= x && x <= xmax
      neigh (y, x) = if green ! (y, x) then [] else filter (\pos -> inBounds pos && not (green ! pos)) [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
      outer = concat ([[(y, 1), (y, xmax)] | y <- [1 .. ymax]] ++ [[(1, x), (ymax, x)] | x <- [1 .. xmax]])
      notGreen pos = not (green ! pos)
      outs = map snd (rights (bfs neigh notGreen (filter notGreen outer)))
      outside :: Array Pos Bool
      outside = accumArray (||) False ((1, 1), (ymax, xmax)) (map (, True) outs)
      inside pos = not (outside ! pos)
      good ((x1, y1), (x2, y2)) = all inside [(top, left), (top, right), (bottom, left), (bottom, right)] && lrInside && topbotInside
        where lrInside = all (\y -> inside (y, left) && inside (y, right)) [top .. bottom]
              topbotInside = all (\x -> inside (top, x) && inside (bottom, x)) [left .. right]
              top = idy IntMap.! min y1 y2
              bottom = idy IntMap.! max y1 y2
              left = idx IntMap.! min x1 x2
              right = idx IntMap.! max x1 x2
      sizes2 = map size (filter good pairs)
  print (maximum sizes2)
