-- http://hackage.haskell.org/package/diffarray-0.1.1/docs/Data-Array-Diff.html
-- https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average
module RayTracer.PhotographicPlate where
import RayTracer.FileToVector (qArray2D, uArray2D)
import qualified Data.Vector.Unboxed as U
import RayTracer.Constants (size)

type PlateVal = (Int, Int, Double)
type Lattice = U.Vector Double

{--
Here there should be a method for averaging
the rays and returning a UArray to publish.

Look at Diff Arrays: Data.Array.Diff for faster updates.
--}

processPlate :: [PlateVal] -> Lattice -> Lattice
processPlate [] ll = ll
processPlate (x:xs) ll = processPlate xs (rayToPlate x ll)

rayToPlate :: PlateVal -> Lattice -> Lattice
rayToPlate (x, y, t) ary =
  let s = mAvg t (qArray2D size (x, y) ary) in
  uArray2D size (x,y) s ary

-- how to choose α? coupon collection?
mAvg :: Double -> Double -> Double
mAvg a 0 = a
mAvg t avg = α * t + (1 - α) * avg
  where α = 0.7
-- mAvg a avg = 0.7 * (avg-a) + avg