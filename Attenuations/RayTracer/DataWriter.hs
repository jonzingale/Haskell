module RayTracer.DataWriter where
import Data.Array.Unboxed -- strict fast Arrays
import System.Random (mkStdGen, randomRs)
import Data.List (sort)

import qualified Data.ByteString.Lex.Fractional as L
import qualified Data.ByteString.Char8 as L
import qualified Data.Vector.Unboxed as U

type ULattice = UArray Int Double
type Lattice = U.Vector Double

randos = randomRs (0, 1::Double).mkStdGen $ 32

-- Produces an empty plate and a stratified data file of given size
dataGeneration :: Int -> IO()
dataGeneration n = do
  saveZeros n
  saveArr ("StratifiedArray3D_" ++ show n) $ stratifiedArray3D n

-- saveArr "GradArray" gradArray => "./Tests/dataGradArray"
saveArr :: String -> ULattice -> IO()
saveArr file ary =
  writeFile ("./Tests/data" ++ file) $ aryToStr ary
  where aryToStr = unlines.(map show).elems

savePlate :: String -> Lattice -> IO()
savePlate filename ary =
  writeFile "./Tests/dataTestTrace" $ aryToStr ary
  where aryToStr = unlines.(map show).(U.toList)

saveZeros :: Int -> IO()
saveZeros n =
  let zeros = take (n^2) $ repeat (0.0::Double) in
  writeFile ("./Tests/dataEmptyAry_" ++ (show n)) $ aryToStr zeros
  where aryToStr = unlines.(map show)

bigSparceArray :: ULattice
bigSparceArray =
  let sparse = randomRs (1::Int, 1000) $ mkStdGen 32 in
  let spRandos = [ if r == 1 then t else 0.0 | (t, r) <- zip randos sparse] in
  let bounds = (0::Int, 10^9-1) in
  listArray bounds spRandos

-- 2D Files
emptyAry :: ULattice
emptyAry = listArray (1::Int, 100^2) $ (take (100^2)).repeat $ 0.0

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

stratifiedArray10K :: ULattice
stratifiedArray10K =
  let grades = take 100 $ randos in
  let bounds = (1::Int, 10^4) in
  let ary = foldr (++) [] $ map sevenOfEm grades in
  listArray bounds ary
  where
    sevenOfEm = (take 100).repeat

stratifiedArray1M :: ULattice
stratifiedArray1M =
  let grades = take 1000 $ randos in
  let bounds = (1::Int, 10^6) in
  let ary = foldr (++) [] $ map sevenOfEm grades in
  listArray bounds ary
  where
    sevenOfEm = (take 1000).repeat

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

-- takes a size and returns a cube.
stratifiedArray3D :: Int -> ULattice
stratifiedArray3D size =
  let grades = take size randos in
  let ary = foldr (++) [] $ map crossSection grades in
  listArray (1::Int, size^3) ary
  where
    crossSection = take (size^2) . repeat