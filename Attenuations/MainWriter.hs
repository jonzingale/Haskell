module Main where
import qualified Data.ByteString.Lex.Fractional as L
import qualified Data.ByteString.Char8 as L
import qualified Data.Vector.Unboxed as U
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
size   ary_size   time
19  MB 100^3      user: 3mins
302 MB 250^3      user: 8mins
2.42GB 500^3      user: 20mins # expect 12 mins
4   GB 1000^3     user: _mins

Exp regression:
a = 10293934.8
b = 1.010689625
y = a*b^500

Log regression:
a = -2.322580826
b = 1.807672315
y = a + b * log x
--}

-- Estimation functions
arySize x =
  let a = 8.657703936 in
  let b = 1.011799709 in
  a * b ** x

aryTime x =
  let a = -22.1294159 in
  let b = 5.45678334 in
  a + b * log x

main = do
    args <- getArgs
    case args of
      [n] -> do
        dataGeneration (read n)
      [] -> do
        dataGeneration 100
      _ -> putStrLn "Wrong number of arguments"

-- Produces an empty plate and a stratified data file of given size
dataGeneration :: Int -> IO()
dataGeneration n = do
  let filename = ("StratifiedArray3D_" ++ show n)
  saveArr filename (stratifiedArray3D n)
  -- saveZeros n

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