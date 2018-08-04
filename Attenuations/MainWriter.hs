module Main where
import Data.Array.Unboxed (UArray, elems, listArray)
import System.Random (mkStdGen, randomRs)
import System.Environment (getArgs)
import Data.List (sort)

type ULattice = UArray Int Double

main = do
    args <- getArgs
    case args of
      [n] -> do
        dataGeneration (read n)
      [] -> do
        dataGeneration 100
      _ -> putStrLn "Wrong number of arguments"

-- Produces a traceable data file of given size and an empty plate 
dataGeneration :: Int -> IO()
dataGeneration n = do
  let filename = ("StratifiedArray3D_" ++ show n)
  saveArr filename (stratifiedArray3D n)
  saveZeros n -- uncomment and recompile for new sizes

-- saveArr "GradArray" gradArray => "./Data/dataGradArray"
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

randos :: [Double]
randos = randomRs (0, 1).mkStdGen $ 32