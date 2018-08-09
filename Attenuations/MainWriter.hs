module Main where
import Data.Array.Unboxed (UArray, elems, listArray)
import System.Directory (doesPathExist, createDirectory)
import System.Random (mkStdGen, randomRs)
import Data.List (sort)

type ULattice = UArray Int Double

main = do -- Write empty plates and test data.
    test100 <- doesPathExist "./Data/dataStratifiedArray3D_100"
    path100 <- doesPathExist "./Data/dataEmptyAry_10000"
    path1K <- doesPathExist "./Data/dataEmptyAry_1000000"
    datDir <- doesPathExist "./Data"
    if datDir
      then putStr "\nData directory exists\n"
      else do
        putStr "creating Data directory"
        createDirectory "./Data"
    if test100
      then putStr "test data: dataStratifiedArray3D_100 exists\n"
      else do
        putStr "creating dataStratifiedArray3D_100"
        saveArr "StratifiedArray3D_100" (stratifiedArray3D 100)

    if path100
      then putStr "empty 100 exists\n"
      else do
        putStr "creating dataEmptyAry_10000"
        saveZeros 100

    if path1K
      then putStr "empty 1k exists\n"
      else do
        putStr "creating dataEmptyAry_1000000"
        saveZeros 1000

saveArr :: String -> ULattice -> IO()
saveArr file ary =
  writeFile ("./Data/data" ++ file) $ aryToStr ary
  where aryToStr = unlines.(map show).elems

saveZeros :: Int -> IO()
saveZeros n =
  let nn = n^2 in
  let zeros = take (nn) $ repeat (0.0::Double) in
  writeFile ("./Data/dataEmptyAry_" ++ (show nn)) $ aryToStr zeros
  where aryToStr = unlines.(map show)

-- takes a size and returns a cube.
stratifiedArray3D :: Int -> ULattice
stratifiedArray3D size =
  let grades = sort $ take size randos in
  let ary = foldr (++) [] $ map crossSection grades in
  listArray (1::Int, size^3) ary
  where
    crossSection = take (size^2) . repeat
    randos = randomRs (0, 1).mkStdGen $ 32
