module RayTracer.ParallelTracer where
import RayTracer.FileToVector
import RayTracer.Transport
import RayTracer.Crossings
import System.Random

import Control.Parallel.Strategies (rdeepseq, parMap)

{--
Single Threaded:
1M rays, 100^3 ~ 15 minutes
1M rays, 50^3  ~ 8 minutes
1M rays, 10^3  ~ 2 minutes

Parallel Threaded:
1M rays, 100^3 ~ 42 secs
--}

allOnes = fileToAry "./Tests/data1M"

totalAttenuation (x, z) (θ, φ) ary =
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
  let results = parMap rdeepseq (thread ary) $ take (10^6) rCoords
  print $ sum results
  where
    thread ary (cs, as) = totalAttenuation cs as ary
