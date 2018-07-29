module RayTracer.ParallelTracer where
import Control.Parallel.Strategies (rdeepseq, parListChunk, rseq, using)
import RayTracer.FileToVector (fileToAry, qArray, qArray2D, displayRange)
import RayTracer.PhotographicPlate (processPlate)
import RayTracer.DataWriter (savePlate)
import RayTracer.Transport (transport)
import RayTracer.GaussianBeam (beam)

-- import qualified Data.Vector.Unboxed as U

{-- Trace times:
Single Threaded interpreted: 1M rays, 100^3 ~ 15 minutes
Single Threaded compiled: 1M rays, 100^3 ~ 42 secs
Eight Threaded compiled: 1M rays, 100^3 ~ 9 secs
--}

{--
distance from source to face and converts mm to units.
a source 1mm distance to the front face is 4 units from the exit.
--}
mmToUnits :: Double -> Double
mmToUnits d  = 2 * d

size = 100 -- lattice side length

attenuation ary ((x, z), (θ, φ)) = -- this could be written better
  let path = takeWhile stopCond $ transport (x, z) (θ, φ) in
  let s = sum [ seg * qArray size ijk ary | (ijk, seg) <- path] in
  let (i,j,k) = fst.last $ path in
  (i, k, s)
  where
    stopCond ((x,y,z), s) =
      x<size && y<size && z<size &&
      x>=0  && y>=0  && z>=0

parallelTrace ary = do
  let gBeams = (beam.mmToUnits) 100 2 -- Distance Deviation
  let rays = map (attenuation ary) gBeams
  let results = rays `using` parListChunk 64 rdeepseq
  return results -- [(x, z, SegmentLength)]

testTrace = do
  emptyAry <- fileToAry "./Tests/dataEmptyAry"
  ary <- fileToAry "./Tests/dataStratifiedArray3D"
  -- trace lattice
  plateAry <- parallelTrace ary
  -- process image file
  let processedPlate = processPlate plateAry emptyAry
  savePlate "tmp" processedPlate
  -- displays dynamic range for new file
  test <- fileToAry "./Tests/dataTestTrace"
  displayRange test


