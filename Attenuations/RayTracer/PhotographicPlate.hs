-- https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average

module RayTracer.PhotographicPlate where
import RayTracer.FileToVector (qArray2D, uArray2D)
import Data.Vector.Unboxed (Vector)
import RayTracer.Constants (size)

type PlateVal = (Int, Int, Double)
type Lattice = Vector Double

processPlate :: [PlateVal] -> Lattice -> Lattice
processPlate [] ll = ll
processPlate (x:xs) ll = processPlate xs (rayToPlate x ll)

rayToPlate :: PlateVal -> Lattice -> Lattice
rayToPlate (x, y, t) ary =
  let s = mAvg t (qArray2D size (x, y) ary) in
  uArray2D size (x,y) s ary

mAvg :: Double -> Double -> Double
mAvg a 0 = a
mAvg t avg = α * t + (1 - α) * avg
  where α = 0.7
