{-# LANGUAGE BinaryLiterals #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Control.Arrow ((***))
import Control.Monad (when)
import Data.Bits ((.&.), (.|.), bit, complement, popCount, shiftL, shiftR, testBit)
import Data.Containers.ListUtils (nubOrd)
import Data.List (foldl', partition)
import Data.Word (Word64)

type Tree = ((Int, Int), [Int])
type Present = [Variant]
type Variant = Word64

fits :: [Present] -> Tree -> Bool
fits _ ((w, h), _) | w * h > 64 = error "TODO: handle size > 64"
fits presents ((w, h), counts) = fits' free0 needs
  where expandedPresents = map (map (expand w)) presents
        needs = concat (zipWith replicate counts expandedPresents)
        free0 = bit (w * h) - 1
        fits' :: Word64 -> [Present] -> Bool
        fits' _ [] = True
        -- the usages of 3 assume each present has a width of exactly 3, otherwise it will miss some possible placements
        fits' free (pres:xs) = or [fits' (free .&. complement mask) xs | variant <- pres, dy <- [0.. h - 3], dx <- [0.. w - 3], let mask = variant `shiftL` (dy * w + dx), free .&. mask == mask]

possible :: [Present] -> Tree -> Bool
possible sizes ((w, h), counts) = sum (zipWith (*) (map (popCount . head) sizes) counts) <= w * h

definitelyPossible :: Tree -> Bool
definitelyPossible ((w, h), xs) = (w `quot` 3) * (h `quot` 3) >= sum xs

expand :: Int -> Variant -> Variant
expand width n = row1 .|. row2 `shiftL` dWidth .|. row3 `shiftL` (dWidth * 2)
  where row1 = n .&. 0b111
        row2 = n .&. 0b111000
        row3 = n .&. 0b111000000
        dWidth = width - 3

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

present :: [String] -> Present
present s = nubOrd (cw4 v0 ++ cw4 (fliph v0))
  where v0 = foldl' bit' 0 (snd (splitOnOne ':' (concat s)))
        bit' acc '.' = acc `shiftL` 1
        bit' acc '#' = acc `shiftL` 1 .|. 1
        bit' _ c = error (c : " invalid present")
        cw4 = take 4 . iterate cw

cw :: Variant -> Variant
cw pres = sum [if pres `testBit` src then bit dst else 0 | (dst, src) <- zip [2, 5, 8, 1, 4, 7, 0, 3, 6] [0..]]

fliph :: Variant -> Variant
fliph pres = right `shiftL` 2 .|. mid .|. left `shiftR` 2
  where right = pres .&. 0b001001001
        mid   = pres .&. 0b010010010
        left  = pres .&. 0b100100100

tree :: String -> Tree
tree = (((read *** read) . splitOnOne 'x') *** (map read . words)) . splitOnOne ':'

main :: IO ()
main = do
  s <- readInputFile
  let presentsTrees = splitOn "" (lines s)
      presents = map present (init presentsTrees)
      trees = map tree (last presentsTrees)
      maybes = filter (possible presents) trees
      (yeses, stillMaybes) = partition definitelyPossible maybes
  print (length yeses)
  print (length maybes)
  --print (count (fits presents) maybes)
  when False $ print (length yeses + count (fits presents) stillMaybes)
