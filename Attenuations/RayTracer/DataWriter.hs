import Data.Array.Unboxed -- strict fast Arrays
import System.Random -- randomRs
import Data.List -- sort

type ULattice = UArray Int Double

randos = randomRs (0, 1::Double).mkStdGen $ 42

-- saveArr "GradArray" gradArray => "./Tests/dataGradArray"
saveArr :: String -> ULattice -> IO()
saveArr file ary =
  writeFile ("./Tests/data" ++ file) $ aryToStr ary
  where aryToStr = unlines.(map show).elems

bigArray :: ULattice
bigArray = listArray bounds randos
  where bounds = (0::Int, 10^6-1)

gradArray :: ULattice
gradArray =
  let grades = sort.take 7 $ randos in
  let bounds = (1::Int, 49) in
  let ary = foldr (++) [] $ map sevenOfEm grades in
  listArray bounds ary
  where
    sevenOfEm = (take 7).repeat

stratifiedArray :: ULattice
stratifiedArray =
  let grades = take 7 $ randos in
  let bounds = (1::Int, 49) in
  let ary = foldr (++) [] $ map sevenOfEm grades in
  listArray bounds ary
  where
    sevenOfEm = (take 7).repeat