-- http://hackage.haskell.org/package/normaldistribution-1.1.0.3/docs/Data-Random-Normal.html
module RayTracer.GaussianBeam where
import Data.Random.Normal
import System.Random

type Ray = (EntryCoords, EntryAngles)
type EntryCoords = (XCoord, ZCoord)
type EntryAngles = (Angle, Angle)
type Distance = Double
type XCoord = Double
type YCoord = Double
type ZCoord = Double
type Angle  = Double

{--
coords: (x, z, θ, φ)
distance: d
density: ρ
radius: r

boundary angle = ∂θ
density and angle: (ρ, d) -> [(x, z, θ, φ)]
--}

{--
normally distributed values about (μ, σ).
small values of σ give sharper peaks.

mkNormals' (2::Double, 0.001) 32

* What radius or deviation covers the lattice face?
* How best to assign angles? distance?

maximum $ take 10000 $ mkNormals' (50::Double, 2) 32
--}
beam :: Distance -> [Ray]
beam d = map (ray d) rDisc

ray :: Distance -> EntryCoords -> Ray
ray d (x, z) = ((x, z), (aTan (x) d, aTan (z) d))
  where
    aTan t d =
      case d of
        0 -> atan (t/10**(-13))
        _ -> atan (t/d)

-- normally distributed disc: take 10 rdiscCarte
rDisc = [(r*cos θ + 50, r*sin θ + 50) | (r, θ) <- zip rs θs]
  where
    rs = mkNormals' (50::Double, 2) 32 -- loose beam
    -- rs = mkNormals' (50::Double, 0.1) 32 -- tight beam
    θs = randomRs (0, 2*pi::Double) $ mkStdGen 32

theta x z d =
  let d' = if d == 0 then 10**(-13) else d in
  atan (x/d')

phi x z d =
  let d' = if d == 0 then 10**(-13) else d in
  pi/2 - atan (z/d')
