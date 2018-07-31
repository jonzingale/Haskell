module Main where
import qualified Data.ByteString.Lex.Fractional as L
import qualified Data.ByteString.Char8 as L
import qualified Data.Vector.Unboxed as U
import Data.Array.Unboxed -- strict fast Arrays
import System.Environment
import System.Random

type ULattice = UArray Int Double

{--
4GB 1/1000 density 10^9 Doubles: real 22m35.839s

Benchmarks:
size  ary_size   time
19 MB 100^3      user: 3mins
302MB 250^3      user: 8mins
1.5GB 500^3      user: _mins # expect 2GB
4  GB 1000^3     user: _mins

Exp regression:
a = 10293934.8
b = 1.010689625
y = a*b^500
--}
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
  saveZeros n
  saveArr ("StratifiedArray3D_" ++ show n) $ stratifiedArray3D n

-- ghc -O2 MainWriter.hs
-- time ./MainWriter bigSparsey

-- rm MainWriter.o MainWriter.hi MainWriter

randos :: [Double]
randos = randomRs (0, 1).mkStdGen $ 32

-- saveArr "GradArray" gradArray => "./Tests/dataGradArray"
saveArr :: String -> ULattice -> IO()
saveArr file ary =
  writeFile ("./Tests/data" ++ file) $ aryToStr ary
  where aryToStr = unlines.(map show).elems

saveZeros :: Int -> IO()
saveZeros n =
  let zeros = take (n^2) $ repeat (0.0::Double) in
  writeFile ("./Tests/dataEmptyAry_" ++ (show n)) $ aryToStr zeros
  where aryToStr = unlines.(map show)

-- takes a size and returns a cube.
stratifiedArray3D :: Int -> ULattice
stratifiedArray3D size =
  let grades = take size randos in
  let ary = foldr (++) [] $ map crossSection grades in
  listArray (1::Int, size^3) ary
  where
    crossSection = take (size^2) . repeat
