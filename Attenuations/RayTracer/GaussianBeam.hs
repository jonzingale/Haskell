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
normally distributed values about (μ, σ).
small values of σ give sharper peaks.

* What radius or deviation covers the lattice face?
* Remember to throw away values outside the lattice.
--}

center = 50

-- parallelize me? see ParallelTracer
-- beam `using` parListChunk 64 rdeepseq
beam :: Distance -> Beam
beam d = map ((ray.λd) d) rDisc

-- ray is derived from a cone with apex distance d
-- from the center. Be sure to rescale the distribution.
ray :: Distance -> EntryCoords -> Ray
ray d (x, z) = ((x*center+center, z*center+center), (aTan x d, aTan z d))
  where
    aTan t d = pi/2 - atan (t/d)

rDisc :: [EntryCoords]
rDisc = [(r*cos θ, r*sin θ) | (r, θ) <- zip rs θs]
  where
    θs = randomRs (0::Double, pi::Double) $ mkStdGen 32
    rs = mkNormals' (0, 1) 32 -- (μ, σ)

{--
Cone Normalization:
The coords are normalized around 0 by default.
ray 1 (1, 0) => (100, 50) for coords
d == 1 => d == 50 cells.
d == 2 from output plane places apex on input plane.

The goal here is to scale the users input distance
to be consistent with the ray tracers internal
representation.

The internal representation assumes the base of
the light cone to be situated at the output plane.
the distance from the output plane to the input
plane is 2 units, ~ 1mm.
--}

{--
Dimensions: 1mm lattice. 6mm distance to point source.
  
6 mm distance from point source to input plane.
14 units from point source to output plane.
--}
λd :: Distance -> Distance
λd d  = 2 * d + 2