import Data.Array.Unboxed -- strict fast Arrays
import System.Random

saveArr = writeFile "./Tests/data" $ unlines.map show $ elems bigArray

bigArray :: UArray Int Double
bigArray = listArray bounds $ randomRs (0, 1::Double).mkStdGen $ 42
  where bounds = (0::Int, 10^6-1)