import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

repeat2 :: (Int, (Int, Int)) -> Int
repeat2 (sz, _) | odd sz = 0
repeat2 (sz, rng) = sumMultiples (10 ^ (sz `quot` 2) + 1) rng

repeatAny :: (Int, (Int, Int)) -> Int
repeatAny (1, _) = 0
repeatAny (2, rng) = sumMultiples 11 rng
repeatAny (3, rng) = sumMultiples 111 rng
repeatAny (4, rng) = sumMultiples 101 rng
repeatAny (5, rng) = sumMultiples 11111 rng
repeatAny (6, rng) = sumMultiples 1001 rng + sumMultiples 10101 rng - sumMultiples 111111 rng
repeatAny (7, rng) = sumMultiples 1111111 rng
repeatAny (8, rng) = sumMultiples 10001 rng
repeatAny (9, rng) = sumMultiples 1001001 rng
repeatAny (10, rng) = sumMultiples 100001 rng + sumMultiples 101010101 rng - sumMultiples 1111111111 rng
repeatAny (n, _) = error ("TODO handle length " ++ show n)

sumMultiples :: Int -> (Int, Int) -> Int
sumMultiples n (l, r) = n * (l' + r') * (r' - l' + 1) `quot` 2
  where l' = let (qt, rm) = l `quotRem` n in qt + (if rm == 0 then 0 else 1)
        r' = r `quot` n

rangesEqualDigits :: (Int, Int) -> [(Int, (Int, Int))]
rangesEqualDigits (l, r) = map rng [lsz .. rsz]
  where rng n = (n, (if n == lsz then l else 10 ^ (n - 1), if n == rsz then r else 10 ^ n - 1))
        lsz = length (show l)
        rsz = length (show r)

range :: String -> (Int, Int)
range s = let (l, r) = splitOnOne '-' s in (read l, read r)

main :: IO ()
main = do
  s <- readInputFile
  let ranges = map range (splitOn ',' s)
      ranges' = concatMap rangesEqualDigits ranges
  print (sum (map repeat2 ranges'))
  print (sum (map repeatAny ranges'))
