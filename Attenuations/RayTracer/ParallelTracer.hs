module RayTracer.ParallelTracer where
import Control.Parallel.Strategies (rdeepseq, parListChunk, rseq, using)
import RayTracer.PhotographicPlate (processPlate)
import RayTracer.FileToVector (fileToAry)
import RayTracer.FileToVector (qArray)
import RayTracer.Transport (transport)
import RayTracer.GaussianBeam (beam)
import System.Random

{--
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

{-- LEGACY:
attenuation ary ((x, z), (θ, φ)) =
  let ijkSeg = takeWhile stopCond $ transport (x, z) (θ, φ) in
  sum [ seg * qArray 100 ijk ary | (ijk, seg) <- ijkSeg]
  where
    stopCond ((x,y,z), s) =
      x<100 && y<100 && z<100 &&
      x>=0  && y>=0  && z>=0

-- TODO: filter non-lattice values
parallelTrace ary = do
  let gBeams = take (10^6) $ beam.mmToUnits $ 1
  let rays = map (attenuation ary) gBeams
  let results = rays `using` parListChunk 64 rdeepseq
  print $ sum results
--}

size = 100

attenuation ary ((x, z), (θ, φ)) = -- this could be written better
  let path = takeWhile stopCond $ transport (x, z) (θ, φ) in
  let s = sum [ seg * qArray size ijk ary | (ijk, seg) <- path] in
  let (i,j,k) = fst.last $ path in
  (i, k, s)
  where
    stopCond ((x,y,z), s) =
      x<size && y<size && z<size &&
      x>=0  && y>=0  && z>=0

-- TODO: filter non-lattice values
parallelTrace ary = do
  let gBeams = take (10^1) $ beam.mmToUnits $ 1
  let rays = map (attenuation ary) gBeams
  let results = rays `using` parListChunk 64 rdeepseq
  return results -- [(x, z, SegmentLength)]

testTrace = do
  emptyAry <- fileToAry "./Tests/dataEmptyAry"
  ary <- fileToAry "./Tests/data1M"
  plateAry <- parallelTrace ary
  print $ processPlate plateAry emptyAry
