import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow (second)
import Data.Map ((!), Map)
import qualified Data.Map as Map

type Server = String

countPathsToOut :: Map Server [Server] -> Map (Server, Bool, Bool) Int
countPathsToOut dsts = cache
  where
    cache = Map.fromList (outs ++ notOuts)
    outs = [(("out", dac, fft), if dac || fft then 0 else 1) | dac <- [False, True], fft <- [False, True]]
    notOuts = [((d, dac, fft), paths d dac fft) | d <- Map.keys dsts, d /= "out", dac <- [False, True], fft <- [False, True]]
    paths "dac" True  fft   = sum [cache ! (d, False, fft) + cache ! (d, True, fft) | d <- dsts ! "dac"]
    paths "dac" False _     = error "asked for dac without visiting it"
    paths "fft" dac   True  = sum [cache ! (d, dac, False) + cache ! (d, dac, True) | d <- dsts ! "fft"]
    paths "fft" _     False = error "asked for fft without visiting it"
    paths src   dac   fft   = sum [cache ! (d, dac, fft) | d <- dsts ! src]

device :: String -> (String, [String])
device = second words . splitOnOne ':'

main :: IO ()
main = do
  s <- readInputFile
  let src = Map.fromList (map device (lines s))
      pathsToOut = countPathsToOut src
  print (sum (Map.elems (Map.filterWithKey (\(x, _, _) _ -> x == "you") pathsToOut)))
  print (Map.findWithDefault 0 ("svr", True, True) pathsToOut)
