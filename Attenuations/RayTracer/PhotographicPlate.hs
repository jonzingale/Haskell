-- http://hackage.haskell.org/package/diffarray-0.1.1/docs/Data-Array-Diff.html

{-# LANGUAGE BangPatterns #-}

module RayTracer.PhotographicPlate where
import qualified Data.ByteString.Lex.Fractional as L
import qualified Data.ByteString.Char8 as L
import qualified Data.Vector.Unboxed as U
import RayTracer.FileToVector

type Dimension = Int -- length of lattice side

type Ray = (EntryCoords, EntryAngles)
type EntryAngles = (Double, Double)
type EntryCoords = (Double, Double)
type Lattice = U.Vector Double

{--
Here there should be a method for averaging
the rays and returning a UArray to publish
as a File.

Look at Diff Arrays: Data.Array.Diff for faster updates.
--}

ary :: IO Lattice
ary = do
  a <- fileToAry "./Tests/data1M" -- :: U.Vector Double
  return a

rayToPlate :: Ray -> Lattice -> Lattice
rayToPlate ray ary =
  let (x, y, t) = (0,0,1) in-- exitValue r # once i can
  let s = mAvg t (qArray2D 1000 (x,y) ary) in
  uArray2D 1000 (x,y) s ary

-- how to choose α? coupon collection?
mAvg :: Double -> Double -> Double
mAvg a b = 0.01 * (b-a) + b
-- exitValue = ?

avg :: [Double] -> Double
avg (x:xs) = a*x + (1-a)*(avg xs)
  -- where a = 0.95 --recommended
  where a = 0.001 -- closer: 0.4896627203851963
avg [] = 0