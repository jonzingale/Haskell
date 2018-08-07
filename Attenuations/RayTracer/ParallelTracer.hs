module RayTracer.ParallelTracer (parallelTrace) where
import Control.Parallel.Strategies (rdeepseq, parListChunk, using)
import RayTracer.FileToVector (qArray)
import RayTracer.Transport (transport)
import RayTracer.GaussianBeam (beam)
import RayTracer.Constants (size)

{--
distance from source to face and converts mm to units.
a source 1mm distance to the front face is 4 units from the exit.
Thus the x2 in the input to the beam.

mmToUnits :: Double -> Double
mmToUnits d  = 2 * d
--}

attenuation ary ((x, z), (θ, φ)) =
  let path = takeWhile stopCond $ transport (x, z) (θ, φ) in
  let s = sum [ seg * qArray size ijk ary | (ijk, seg) <- path] in
  let (i,j,k) = fst.last $ path in (i, k, s)
  where
    stopCond ((x,y,z), s) =
      x<size && y<size && z<size &&
      x>=0 && z>=0

parallelTrace ary = do
  let gBeams = beam (2 * 10^3) 1 -- Distance Deviation
  let rays = map (attenuation ary) gBeams
  let results = rays `using` parListChunk 64 rdeepseq
  return results -- [(x, z, SegmentLength)]
