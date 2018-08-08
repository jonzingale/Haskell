-- https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average

module RayTracer.PhotographicPlate where
import RayTracer.FileToVector (qArray2D) -- , uArray2D)
import qualified Data.Vector.Unboxed as U
import RayTracer.Constants (size)

type PlateVal = (Int, Int, Double)
type Lattice = U.Vector Double

-- processPlate :: [PlateVal] -> Lattice -> Lattice
-- processPlate [] ll = ll
-- processPlate (x:xs) ll = processPlate xs (rayToPlate x ll)

-- rayToPlate :: PlateVal -> Lattice -> Lattice
-- rayToPlate (x, y, t) ary =
--   let s = mAvg t (qArray2D size (x, y) ary) in
--   uArray2D size (x,y) s ary

processPlate :: [PlateVal] -> Lattice -> Lattice
processPlate xs ll =
  (U.//) ll [(x + z * size, mAvg t (qArray2D size (x, z) ll)) | (x,z,t) <- xs]

mAvg :: Double -> Double -> Double
mAvg a 0 = a
mAvg t avg = α * t + (1 - α) * avg
  where α = 0.7