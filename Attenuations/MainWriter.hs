module Main where
import qualified Data.ByteString.Lex.Fractional as L
import qualified Data.ByteString.Char8 as L
import qualified Data.Vector.Unboxed as U
import Data.Array.Unboxed -- strict fast Arrays
import System.Environment
import System.Random

type ULattice = UArray Int Double

-- ghc -O2 MainWriter.hs
-- time ./MainWriter bigSparsey

-- rm MainWriter.o MainWriter.hi MainWriter

randos :: [Double]
randos = randomRs (0, 1).mkStdGen $ 32

saveArr :: String -> ULattice -> IO()
saveArr file ary =
  writeFile ("./Tests/data" ++ file) $ aryToStr ary
  where aryToStr = unlines.(map show).elems

bigSparceArray :: ULattice
bigSparceArray =
  let sparse = randomRs (1::Int, 1000) $ mkStdGen 32 in
  let spRandos = [ if r == 1 then t else 0.0 | (t, r) <- zip randos sparse] in
  let bounds = (0::Int, 10^9-1) in
  listArray bounds spRandos

main = do
    args <- getArgs
    case args of
      [string] -> do
        saveArr string bigSparceArray
      _ -> putStrLn "Wrong number of arguments"
