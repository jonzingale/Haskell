module Main where
import Data.Array.Unboxed (UArray, elems, listArray)
import System.Environment
import Data.List (sort)
import System.Random

type ULattice = UArray Int Double

{--
To Compile and Run:
ghc -O2 MainWriter.hs
time ./MainWriter 500
rm MainWriter.o MainWriter.hi MainWriter

4GB 1/1000 density 10^9 Doubles: real 22m35.839s

Benchmarks for running Main:
size   ary_size   time         rays
19  MB 100^3      user: 3mins  1M
302 MB 250^3      user: 8mins  1M
2.42GB 500^3      user: 30mins 3M
6.65GB 700^3      user: 40mins 1M
19.4GB 1000^3     user: 80mins 1M
--}

-- Estimation functions
arySize x = -- for 1000, size -> 1 terrabyte
  let a = 8657703.9 in
  let b = 1.011799709 in
  a * b ** x

aryTime x = -- for 1000, time -> 63 mins
  let a = -141.595473 in
  let b = 9.90916174 in
  a + b * log x

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

-- saveArr "GradArray" gradArray => "./Tests/dataGradArray"
saveArr :: String -> ULattice -> IO()
saveArr file ary =
  writeFile ("./Tests/data" ++ file) $ aryToStr ary
  where aryToStr = unlines.(map show).elems

saveZeros :: Int -> IO()
saveZeros n =
  let nn = n^2 in
  let zeros = take (nn) $ repeat (0.0::Double) in
  writeFile ("./Tests/dataEmptyAry_" ++ (show nn)) $ aryToStr zeros
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