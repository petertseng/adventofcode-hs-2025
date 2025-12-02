import AdventOfCode (readInputFile)

zeroes :: Int -> Int -> Int
-- quot not div: want -1 / -100 to be 0, not 1
zeroes prev pos | pos <= 0 = pos `quot` (-100) + (if prev `rem` 100 == 0 then 0 else 1)
zeroes _ pos = pos `quot` 100

rot :: String -> Int
rot ('L':s) = -read s
rot ('R':s) = read s
rot s = error ("bad " ++ s)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let rots = map rot (lines s)
      -- mod not rem: need the negatives to become positive.
      points = scanl (\acc x -> acc `mod` 100 + x) 50 rots
  print (count ((== 0) . (`rem` 100)) points)
  print (sum (zipWith zeroes points (drop 1 points)))
