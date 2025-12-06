import AdventOfCode (readInputFile)

import Data.List (transpose)

math :: (Int -> Int -> Int) -> [String] -> Int
math f = foldl1 f . map read

splitIf :: (a -> Bool) -> [a] -> [[a]]
splitIf f s = case dropWhile f s of
  [] -> []
  s' -> w : splitIf f s''
        where (w, s'') = break f s'

op :: String -> Int -> Int -> Int
op "*" = (*)
op "+" = (+)
op s = error ("invalid op " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let operandsRaw = init (lines s)
      ops = map op (words (last (lines s)))
      operands1 = transpose (map words operandsRaw)
      operands2 = splitIf (all (== ' ')) (transpose operandsRaw)
  mapM_ (print . sum . zipWith math ops) [operands1, operands2]
