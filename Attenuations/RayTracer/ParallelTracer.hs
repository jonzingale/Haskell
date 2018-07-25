module RayTracer.ParallelTracer where
import Control.Parallel.Strategies (rdeepseq, parListChunk, rseq, using)
import RayTracer.FileToVector (qArray)
import RayTracer.Transport (transport)
import RayTracer.GaussianBeam (beam)
import System.Random

{--
Single Threaded interpreted: 1M rays, 100^3 ~ 15 minutes
Single Threaded compiled: 1M rays, 100^3 ~ 42 secs
Eight Threaded compiled: 1M rays, 100^3 ~ 9 secs
--}
import RayTracer.FileToVector (fileToAry)

testTrace = do
  ary <- fileToAry "./Tests/dataMillionOnes"
  let gBeams = take (10^2) $ beam.mmToUnits $ 1
  let rays = map (attenuation ary) gBeams
  let results = rays `using` parListChunk 64 rdeepseq
  print $ sum results

{--
distance from source to face and converts mm to units.
a source 1mm distance to the front face is 4 units from the exit.
--}
mmToUnits :: Double -> Double
mmToUnits d  = 2 * d

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
