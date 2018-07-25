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

-- 2D Files
tenThousandOnes :: ULattice
tenThousandOnes = listArray (1::Int, 10^4) $ (take 10000).repeat $ 1.0

allOnes2D :: ULattice
allOnes2D = listArray (1::Int, 100) $ (take 100).repeat $ 1.0

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

stratifiedArray100 :: ULattice
stratifiedArray100 =
  let grades = take 10 $ randos in
  let bounds = (1::Int, 100) in
  let ary = foldr (++) [] $ map sevenOfEm grades in
  listArray bounds ary
  where
    sevenOfEm = (take 7).repeat

-- 3D Files
millionOnes :: ULattice
millionOnes = listArray (1::Int, 10^6) $ take (10^6).repeat $ 1.0

allOnes3D :: ULattice
allOnes3D = listArray (1::Int, 343) $ (take 343).repeat $ 1.0

gradArray3D :: ULattice
gradArray3D =
  let grades = sort.take 7 $ randos in
  let bounds = (1::Int, 343) in
  let ary = foldr (++) [] $ map sevenOfEm grades in
  listArray bounds ary
  where
    sevenOfEm = (take 7).repeat

stratifiedArray3D :: ULattice
stratifiedArray3D =
  let grades = take 7 $ randos in
  let bounds = (1::Int, 343) in
  let ary = foldr (++) [] $ map sevenOfEm grades in
  listArray bounds ary
  where
    sevenOfEm = (take 7).repeat