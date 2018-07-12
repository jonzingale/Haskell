module RayTracer.GaussianBeam where
import System.Random

type Perspective = (EntryCoords, EntryAngles)
type EntryCoords = (XCoord, ZCoord)
type EntryAngles = (Angle, Angle)
type Distance = Double
type XCoord = Double
type YCoord = Double
type ZCoord = Double
type Angle  = Double

{--
Perhaps build a file contain data for a conic projection.
Perhaps write a random number generator using system data
and producing a gaussian process.

Then there likely should be an affine or projective
transformation passing the conic to the interface.

coords: (x, z, θ, φ)
center: (x', z')
distance: d
density: ρ
radius: r
--}

beam :: Perspective -> Distance -> [Perspective]
beam ((x, z), (θ, φ)) d = [((x, z), (θ, φ))]

-- convert to conic: take 10 rdiscCarte
rdiscCarte = [(r*cos θ, r*sin θ) | (r,θ) <- zip rs θs]
  where
    rs = map sqrt $ randomRs (0, 1::Double) $ mkStdGen 32
    θs = randomRs (0, 2*pi::Double) $ mkStdGen 32