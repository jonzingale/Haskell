import Data.Array.Unboxed -- strict fast Arrays
import System.Random -- randomRs
import Data.List -- sort

randos = randomRs (0, 1::Double).mkStdGen $ 42

-- saveArr "gradArray" gradArray
saveArr :: String -> UArray Int Double -> IO()
saveArr file ary = writeFile ("./Tests/" ++ file) $
          unlines.map show $ elems ary

bigArray :: UArray Int Double
bigArray = listArray bounds randos
  where bounds = (0::Int, 10^6-1)

gradArray :: UArray Int Double
gradArray =
  let grades = sort.take 7 $ randos in
  let bounds = (1::Int, 49) in
  let ary = foldr (++) [] $ map sevenOfEm grades in
  listArray bounds ary
  where
    sevenOfEm = (take 7).repeat