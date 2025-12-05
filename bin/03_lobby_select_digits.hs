import AdventOfCode (readInputFile)

import Data.Char (digitToInt)

maxNum :: Int -> [Int] -> Int
maxNum 0 xs = maximum xs
maxNum n xs = m * 10 ^ n + maxNum (n - 1) (drop (abs i) xs)
  where (m, i) = maximum (zip (dropEnd n xs) [-1, -2..])

dropEnd :: Int -> [a] -> [a]
dropEnd i xs
    | i <= 0 = xs
    | otherwise = f xs (drop i xs)
    where f (x:xs') (_:ys) = x : f xs' ys
          f _ _ = []

main :: IO ()
main = do
  s <- readInputFile
  let batteryBanks = map (map digitToInt) (lines s)
  print (sum (map (maxNum 1) batteryBanks))
  print (sum (map (maxNum 11) batteryBanks))
