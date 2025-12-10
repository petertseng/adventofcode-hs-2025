import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Monad (replicateM)
import Data.Bits (bit, xor)
import Data.Int (Int16)
import Data.List (dropWhileEnd, find, foldl')
import Data.Maybe (fromJust)
import Data.SBV ((.>=), (.==), OptimizeResult(LexicographicResult), OptimizeStyle(Lexicographic), constrain, getModelValue, minimize, optimize, sInteger_)

type Circuit = (Int, [[Int]], [Int])

presses :: Circuit -> Int
presses (target, buttons, _) = fromJust (find canPressN [1 .. length buttons])
  where canPressN n = any ((== target) . foldl' xor 0) (combinations n buttons')
        buttons' = map (sum . map bit) buttons

presses2 :: Circuit -> IO Int16
presses2 (_, buttons, jolts) =
  do
    opt <- optimize Lexicographic $ do
      obuts <- replicateM (length buttons) sInteger_
      mapM_ (\x -> constrain (x .>= 0)) obuts
      mapM_ (\(j, i) -> constrain ((sum [obut | (obut, but) <- zip obuts buttons, i `elem` but]) .== fromIntegral j)) (zip jolts [0..])
      minimize "presses" (sum obuts)
    case opt of
      LexicographicResult r -> return (fromJust (getModelValue "presses" r))
      _ -> error "wrong result"

-- https://rosettacode.org/wiki/Combinations#Haskell
combinations :: Int -> [a] -> [[a]]
combinations m xs = bySize xs !! m
 where
   bySize = foldr f ([[]] : repeat [])
   f x next = zipWith (++) (map (map (x:)) ([]:next)) next

between :: Char -> Char -> (String -> a) -> String -> a
between l r f (c:s) | c == l = f (dropWhileEnd (== r) s)
between l _ _ s = error ("doesn't start with " ++ l : ": " ++ s)

circuit :: String -> Circuit
circuit s = case words s of
  [] -> error "no circuit"
  x:xs -> (between '[' ']' (sum . zipWith bit' [0..]) x, map (between '(' ')' (map read . splitOn ',')) (init xs), between '{' '}' (map read . splitOn ',') (last xs))
  where bit' _ '.' = 0
        bit' i '#' = bit i
        bit'  _  _ = error ("invalid lights " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let circs = map circuit (lines s)
  print (sum (map presses circs))
  v <- mapM presses2 circs
  print (sum v)
