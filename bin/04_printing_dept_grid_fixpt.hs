import AdventOfCode (readInputFile)

import Data.Containers.ListUtils (nubOrd)
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

removable :: Set Pos -> [Pos] -> [Pos]
removable s = filter (\pos -> count (`Set.member` s) (neigh pos) < 4)

remove :: (Set Pos, [Pos]) -> (Set Pos, [Pos])
remove (towel, active) = (towel', active')
  where towel' = towel `Set.difference` Set.fromList removed
        active' = nubOrd (concatMap (filter (`Set.member` towel') . neigh) removed)
        removed = removable towel active

neigh :: Pos -> [Pos]
neigh (y, x) = [(y + dy, x + dx) | dy <- [-1, 0, 1], dx <- [-1, 0, 1], dy /= 0 || dx /= 0]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

paper :: Char -> Bool
paper '@' = True
paper '.' = False
paper c = error (c : " neither @ nor .")

main :: IO ()
main = do
  s <- readInputFile
  let towel = Set.fromList [pos | (pos, c) <- enumGrid (lines s), paper c]
      -- can save a round trip by defining active first before towel,
      -- but this way more accurately reflects intent:
      -- logically, active is derived from towel, not the other way around
      active = Set.toList towel
  print (length (removable towel active))
  let (final, _) = until (null . snd) remove (towel, active)
  print (length towel - length final)
