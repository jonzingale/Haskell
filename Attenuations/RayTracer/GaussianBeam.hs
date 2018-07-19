-- http://hackage.haskell.org/package/normaldistribution-1.1.0.3/docs/Data-Random-Normal.html
module RayTracer.GaussianBeam where
import Data.Random.Normal
import System.Random

type Beam = [Ray]
type Ray = (EntryCoords, (Double, Double))
type EntryCoords = (Double, Double)
type Distance = Double
type Center = Double

{--
normally distributed values about (μ, σ).
small values of σ give sharper peaks.

mkNormals' (2::Double, 0.001) 32

* What radius or deviation covers the lattice face?
* Remember to throw away values outside the lattice.
It may be best to hard code the center at 500.
--}

beam :: Distance -> Center -> Beam
beam d c = map (ray d c) $ rDisc c

-- ray is derived from a cone with apex distance d from the center c
ray :: Distance -> Center -> EntryCoords -> Ray
ray d c (x, z) = ((x, z), (aTan (x-c) d, aTan (z-c) d))
  where
    aTan t d | d == 0 = aTan t $ 10**(-20) -- see prop_AngleSpraysAway test
             | t >= 0 = atan (d/t)
             | otherwise = pi/2 - atan (t/d)

rDisc :: Center -> [EntryCoords]
rDisc c = [(r*cos θ + c, r*sin θ + c) | (r, θ) <- zip (rs c) θs]
  where
    θs = randomRs (0, 2*pi::Double) $ mkStdGen 32
    rs c = mkNormals' (c::Double, 2) 32 -- loose beam
    -- rs = mkNormals' (50::Double, 0.1) 32 -- tight beam

