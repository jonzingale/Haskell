-- http://hackage.haskell.org/package/normaldistribution-1.1.0.3/docs/Data-Random-Normal.html
module RayTracer.GaussianBeam where
import Control.Parallel.Strategies (rdeepseq, parListChunk, rseq, using)
import RayTracer.CumulativeDistribution (neededRays)
import Data.Random.Normal
import System.Random

type Ray = (EntryCoords, EntryAngles)
type EntryAngles = (Double, Double)
type EntryCoords = (Double, Double)
type Deviation = Double
type Distance = Double
type Center = Double
type Beam = [Ray]

-- center = 50
-- center = 125
center = 250
size = 3*10**6 -- number of rays desired, coupon collection?

{--
Cone Normalization:
The coords are normalized around 0 by default.
ray 1 (1, 0) => (100, 50) for coords
d == 1 => d == 50 cells.
d == 2 from output plane places apex on input plane.

The goal here is to scale the users input distance
to be consistent with the ray tracers internal
representation.

Dimensions: 1mm lattice. 6mm distance to point source.
6 mm distance from point source to input plane.
14 units from point source to output plane.

The internal representation assumes the base of
the light cone to be situated at the output plane.
the distance from the output plane to the input
plane is 2 units, ~ 1mm.
--}

beam :: Distance -> Deviation -> Beam
beam d σ =
  let needed = neededRays size σ in
  filter posiCond $ take needed $ map (ray d) (rDisc σ) -- rays in mm
  where
    posiCond ((x,z),(_,_)) = x >= 0 && z >= 0 &&
                             x <= 2*center && z <= 2*center
{--
ray is derived from a cone with apex-
distance d standard units from the center.
radius scaled to front plane from exit plane.
front face is 2 units from exit face.
--}
ray :: Distance -> EntryCoords -> Ray
ray d (x, z) = ((coords x (d/2), coords z (d/2)), (angles x d, angles z d))
  where
    -- @(x > 0, d == 0) => 1.1071487177940904
    -- @(x < 0, d == 0) => 2.0344439357957027
    coords t d = center * (t * d / (d + 1) + 1)
    angles t d | t >= 0    = atan ((d+2)/t)
               | otherwise = atan ((d+2)/t) + pi

rDisc :: Deviation -> [EntryCoords]
rDisc σ = [(r*cos θ, r*sin θ) | (r, θ) <- zip (rs σ) θs]
  where
    θs = randomRs (0::Double, pi::Double) $ mkStdGen 32
    rs σ = mkNormals' (0, σ) 31 -- (μ, σ)

