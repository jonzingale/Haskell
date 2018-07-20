-- http://hackage.haskell.org/package/normaldistribution-1.1.0.3/docs/Data-Random-Normal.html
module RayTracer.GaussianBeam where
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
It may be best to hard code the center at 500.
--}

beam :: Distance -> Center -> Beam
beam d c = map (ray d c) rDisc

-- ray is derived from a cone with apex distance d
-- from the center c be sure to rescale the distribution.
ray :: Distance -> Center -> EntryCoords -> Ray
ray d c (x, z) = ((x*c+c, z*c+c), (aTan x d, aTan z d))
  where
    aTan t d | t >= 0 = atan (d/t)
             | otherwise = pi/2 - atan (t/d)

rDisc :: [EntryCoords]
rDisc = [(r*cos θ, r*sin θ) | (r, θ) <- zip rs θs]
  where
    θs = randomRs (0::Double, 2*pi::Double) $ mkStdGen 32
    rs = mkNormals' (0, 2) 32 -- (μ, σ) loose beam

