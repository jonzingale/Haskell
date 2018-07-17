-- http://hackage.haskell.org/package/normaldistribution-1.1.0.3/docs/Data-Random-Normal.html
module RayTracer.GaussianBeam where
import Data.Random.Normal
import System.Random

type Ray = (EntryCoords, (Double, Double))
type EntryCoords = (Double, Double)
type Distance = Double
type Center = Double

{--
normally distributed values about (μ, σ).
small values of σ give sharper peaks.

mkNormals' (2::Double, 0.001) 32

* What radius or deviation covers the lattice face?
--}

beam :: Distance -> Center -> [Ray]
beam d c = map (ray d) $ rDisc c

ray :: Distance -> EntryCoords -> Ray
ray d (x, z) = ((x, z), (aTan (x) d, aTan (z) d))
  where
    aTan t d =
      case d of
        0 -> atan $ t / 10**(-13)
        _ -> atan $ t / d

rDisc :: Center -> [EntryCoords]
rDisc c = [(r*cos θ + c, r*sin θ + c) | (r, θ) <- zip (rs c) θs]
  where
    θs = randomRs (0, 2*pi::Double) $ mkStdGen 32
    rs c = mkNormals' (c::Double, 2) 32 -- loose beam
    -- rs = mkNormals' (50::Double, 0.1) 32 -- tight beam

