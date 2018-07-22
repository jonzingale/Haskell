-- http://hackage.haskell.org/package/normaldistribution-1.1.0.3/docs/Data-Random-Normal.html
module RayTracer.GaussianBeam where
import Control.Parallel.Strategies (rdeepseq, parListChunk, rseq, using)
import Data.Random.Normal
import System.Random

type Ray = (EntryCoords, EntryAngles)
type EntryAngles = (Double, Double)
type EntryCoords = (Double, Double)
type Distance = Double
type Center = Double
type Beam = [Ray]

{--
the coords are normalized around 0 by default.

normally distributed values about (μ, σ).
small values of σ give sharper peaks.

* What radius or deviation covers the lattice face?
* Remember to throw away values outside the lattice.
* It may be best to hard code the center at 500.
--}

center = 50

-- parallelize me? see ParallelTracer
-- beam `using` parListChunk 64 rdeepseq
beam :: Distance -> Beam
beam d = map (ray d) rDisc

-- ray is derived from a cone with apex distance d
-- from the center c be sure to rescale the distribution.
ray :: Distance -> EntryCoords -> Ray
ray d (x, z) = ((x*center+center, z*center+center), (aTan x d, aTan z d))
  where
    aTan t d = pi/2 - atan (t/d)

rDisc :: [EntryCoords]
rDisc = [(r*cos θ, r*sin θ) | (r, θ) <- zip rs θs]
  where
    θs = randomRs (0::Double, pi::Double) $ mkStdGen 32
    rs = mkNormals' (0, 0.1) 32 -- (μ, σ)

