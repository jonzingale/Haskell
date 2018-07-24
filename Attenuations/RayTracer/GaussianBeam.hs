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

{-- Legacy:
distance from source to face and converts mm to units.
a source 1mm distance to the front face is 4 units from the exit.
mmToUnits :: Distance -> Distance
mmToUnits d  = 2 * d + 2

ray :: Distance -> EntryCoords -> Ray
ray d' (x, z) =
  let d = d' / (d' + 1) in -- scales radius to front plane.
  ((coords x d, coords z d), (angles x d', angles z d'))
  where
    -- @d == 0 => atan 0.5 == 0.46364760900080615
    coords t d = t * d * center + center
    angles t d = pi/2 - atan (t/(d+0.5))
--}

center = 50
halfPi = 1.5707963267948966

beam :: Distance -> Beam
beam d = map (ray d) rDisc -- rays in mm

-- ray is derived from a cone with apex-
-- distance d standard units from the center.
-- radius scaled to front plane from exit plane.
ray :: Distance -> EntryCoords -> Ray
ray d (x, z) = ((coords x d, coords z d), (angles x d, angles z d))
  where
    -- @d == 0 => atan 0.5 == 0.46364760900080615: 1/2 rise to run
    coords t d = center * t * d / (d + 1) + center
    angles t d = halfPi - atan (t/(d+0.5))


rDisc :: [EntryCoords]
rDisc = [(r*cos θ, r*sin θ) | (r, θ) <- zip rs θs]
  where
    θs = randomRs (0::Double, pi::Double) $ mkStdGen 32
    rs = mkNormals' (0, 1) 32 -- (μ, σ)

