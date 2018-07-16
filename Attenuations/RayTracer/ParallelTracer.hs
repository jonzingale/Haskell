module RayTracer.ParallelTracer where
import Control.Parallel.Strategies (rdeepseq, parListChunk, rseq, using)
import RayTracer.FileToVector (qArray)
import RayTracer.Transport (transport)
import System.Random

{--
Single Threaded interpreted: 1M rays, 100^3 ~ 15 minutes
Single Threaded compiled: 1M rays, 100^3 ~ 42 secs
Eight Threaded compiled: 1M rays, 100^3 ~ 9 secs
--}

attenuation ary ((x, z), (θ, φ)) =
  let ijkSeg = takeWhile stopCond $ transport (x, z) (θ, φ) in
  sum [ seg * qArray 100 ijk ary | (ijk, seg) <- ijkSeg]
  where
    stopCond ((x,y,z), s) =
      x<100 && y<100 && z<100 &&
      x>=0  && y>=0  && z>=0

rCoords =
  let [a, b, c, d] = take 4 $ randomRs (3, 100) $ mkStdGen 78 in
  let xs = randomRs (0, 100::Double) $ mkStdGen a in
  let zs = randomRs (0, 100::Double) $ mkStdGen b in
  let θs = randomRs (0, pi::Double)  $ mkStdGen c in
  let φs = randomRs (0, pi::Double)  $ mkStdGen d in
  zip (zip xs zs) (zip θs φs)

parallelTrace ary = do
  let coords = take (10^6) rCoords
  let rays = map (attenuation ary) coords
  let results = rays `using` parListChunk 64 rdeepseq
  print $ sum results
