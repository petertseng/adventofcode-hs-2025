{-# LANGUAGE FlexibleContexts #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (getElems, newArray, newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.List (foldl', sortOn, tails)

type Pos = (Int, Int, Int)

sizesAndLast :: (Int, Int) -> [(Int, Int)] -> ((Int, Int, Int), (Int, Int))
sizesAndLast bounds@(minId, maxId) pairs = runST $ do
  parent <- newListArray bounds [minId .. maxId] :: ST s (STUArray s Int Int)
  size   <- newArray bounds 1 :: ST s (STUArray s Int Int)

  let find x = do
        parX <- readArray parent x
        if x /= parX
          then do
            parParX <- find parX
            writeArray parent x parParX
            return parParX
          else return x

  let union x y = do
        parX <- find x
        parY <- find y
        when (parX /= parY) $ do
          sizeX <- readArray size parX
          sizeY <- readArray size parY
          case compare sizeX sizeY of
            LT -> do
              writeArray parent parX parY
              writeArray size parY (sizeX + sizeY)
              writeArray size parX 0
            _ -> do
              writeArray parent parY parX
              writeArray size parX (sizeX + sizeY)
              writeArray size parY 0
        return (parX /= parY)

  let unionN 0 _ prev = return prev
      unionN _ [] prev = return prev
      unionN n ((x, y):xys) prev = do
        success <- x `union` y
        if success then unionN (n - 1) xys (x, y) else unionN n xys prev

  lastPair <- unionN (maxId - minId) pairs undefined
  sizes <- getElems size

  return (top3 sizes, lastPair)

dist :: Pos -> Pos -> Int
dist (a, b, c) (d, e, f) = sum (map (\x -> x * x) [a - d, b - e, c - f])

top3 :: [Int] -> (Int, Int, Int)
top3 = foldl' step (0, 0, 0)
  where step (a, b, c) x
          | x > a = (x, a, b)
          | x > b = (a, x, b)
          | x > c = (a, b, x)
          | otherwise = (a, b, c)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

point :: String -> Pos
point s = case splitOn ',' s of
  [a, b, c] -> (read a, read b, read c)
  _ -> error ("wrong number of commas" ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let points = zip [0..] (map point (lines s))
      pairs = [(x, y) | x:ys <- tails points, y <- ys]
      sorted = sortOn (\((_, x), (_, y)) -> dist x y) pairs
      sortedIds = map (\((x, _), (y, _)) -> (x, y)) sorted
      v = if length points == 20 then 10 else 1000
      ((a, b, c), _) = sizesAndLast (0, length points - 1) (take v sortedIds)
  print (a * b * c)
  let (_, (lid1, lid2)) = sizesAndLast (0, length points - 1) sortedIds
  print (fst3 (snd (points !! lid1)) * fst3 (snd (points !! lid2)))
