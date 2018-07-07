module RayTracer.ParallelTracer where
import Control.Parallel.Strategies (rdeepseq, parMap)
import RayTracer.FileToVector (qArray, fileToAry)
import RayTracer.Transport (transport)
import System.Random

{--
Single Threaded: 1M rays, 100^3 ~ 15 minutes
Parallel Threaded: 1M rays, 100^3 ~ 42 secs
--}

allOnes = fileToAry "./Tests/data1M"

attenuation ary ((x, z), (θ, φ)) =
  let ijkSeg = transport (x, z) (θ, φ) in
  sum [ seg * qArray 100 ijk ary | (ijk, seg) <- takeWhile stopCond ijkSeg]
  where
    stopCond ((x,y,z), s) =
      x<100 && y<100 && z<100 &&
      x>=0 && y>=0 && z>=0

rCoords =
  let [a, b, c, d] = take 4 $ randomRs (3, 100) $ mkStdGen 78 in
  let xs = randomRs (0, 100::Double) $ mkStdGen a in
  let zs = randomRs (0, 100::Double) $ mkStdGen b in
  let θs = randomRs (0, pi::Double)  $ mkStdGen c in
  let φs = randomRs (0, pi::Double)  $ mkStdGen d in
  zip (zip xs zs) (zip θs φs)

parallelTrace = do
  ary <- allOnes
  let coords = take (10^6) rCoords
  let results = parMap rdeepseq (attenuation ary) coords
  print $ sum results
