-- https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average

module RayTracer.PhotographicPlate where
import RayTracer.FileToVector (qArray2D)
import qualified Data.Vector.Unboxed as U
import RayTracer.Constants (size)

type PlateVal = (Int, Int, Double)
type Lattice = U.Vector Double

processPlate :: [PlateVal] -> Lattice -> Lattice
processPlate xs ll =
  let s x z t = mAvg t (qArray2D size (x, z) ll) in
  (U.//) ll [(x + z * size, s x z t) | (x,z,t) <- xs]

mAvg :: Double -> Double -> Double
mAvg a 0 = a
mAvg t avg = α * t + (1 - α) * avg
  where α = 0.7