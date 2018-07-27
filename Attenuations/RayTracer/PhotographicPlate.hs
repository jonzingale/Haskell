-- http://hackage.haskell.org/package/diffarray-0.1.1/docs/Data-Array-Diff.html

{-# LANGUAGE BangPatterns #-}

module RayTracer.PhotographicPlate where
import qualified Data.ByteString.Lex.Fractional as L
import qualified Data.ByteString.Char8 as L
import qualified Data.Vector.Unboxed as U
import RayTracer.FileToVector
import RayTracer.Transport

type Dimension = Int -- length of lattice side

type Ray = (EntryCoords, EntryAngles)
type EntryAngles = (Double, Double)
type EntryCoords = (Double, Double)
type Lattice = U.Vector Double
type PlateVal = (Int, Int, Double)

{--
Here there should be a method for averaging
the rays and returning a UArray to publish
as a File.

Look at Diff Arrays: Data.Array.Diff for faster updates.
--}

size = 100 -- TODO: make 1000

processPlate :: [PlateVal] -> Lattice -> Lattice
processPlate [] ll = ll
processPlate (x:xs) ll = processPlate xs (rayToPlate x ll)


rayToPlate :: PlateVal -> Lattice -> Lattice
rayToPlate (x, y, t) ary =
  let s = mAvg t (qArray2D size (x,y) ary) in
  uArray2D size (x,y) s ary

-- how to choose α? coupon collection?
mAvg :: Double -> Double -> Double
mAvg a b = 0.01 * (b-a) + b

avg :: [Double] -> Double
avg (x:xs) = a*x + (1-a)*(avg xs)
  -- where a = 0.95 --recommended
  where a = 0.001 -- closer: 0.4896627203851963
avg [] = 0