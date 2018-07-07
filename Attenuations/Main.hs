module Main where
import RayTracer.FileToVector
import RayTracer.Transport
import RayTracer.Crossings
import RayTracer.ParallelTracer
import System.Random

{--
Todo:
* Simulate 1M rays from gaussian point source
* Parallelize code
* Monadic Run?
* Tests: Verify / Validate
* Is Strictness on FileToVector valuable?

To Run:
ghc -O2 --make Main.hs -threaded -rtsopts
./Main +RTS -N4

To Clear:
rm Main.o Main.hi Main
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

main = do
  pTrace
