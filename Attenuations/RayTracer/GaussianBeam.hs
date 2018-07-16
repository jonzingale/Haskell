
-- http://hackage.haskell.org/package/normaldistribution-1.1.0.3/docs/Data-Random-Normal.html
module RayTracer.GaussianBeam where
import Data.Random.Normal
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

maximum $ take 10000 $ mkNormals' (50::Double, 12) 32
99.44566204060355
--}

cheapAngles = do
  cheapθs
  cheapφs

cheapθs = do
  let ins = [(0,1,1),(0,1,0),(0,-1,1),(0,-1,0),(1,0,1),(1,0,0),(-1,0,1),(-1,0,0)]
  let vals = [(x, z, d, theta x z d) | (x,z,d)<-ins]
  putStr "X Z D θ\n"
  putStr.unlines.(map show) $ vals
  putStr "\n"

cheapφs = do
  let ins = [(0,1,1),(0,1,0),(0,-1,1),(0,-1,0),(1,0,1),(1,0,0),(-1,0,1),(-1,0,0)]
  let vals = [(x, z, d, phi x z d) | (x,z,d)<-ins]
  putStr "X Z D φ\n"
  putStr.unlines.(map show) $ vals
  putStr "\n"

beam :: Perspective -> Distance -> [Perspective]
beam ((x, z), (θ, φ)) d = [((x, z), (θ, φ))]

-- normally distributed disc: take 10 rdiscCarte
rDisc = [(r*cos θ, r*sin θ) | (r, θ) <- zip rs θs]
  where
    rs = mkNormals' (50::Double, 0.1) 32
    θs = randomRs (0, 2*pi::Double) $ mkStdGen 32

norm x z = sqrt $ x^2 + z^2

theta x z d =
  let x' = if x == 0 then 10**(-13) else x in
  pi/2 - atan (norm x z / d) * cos (atan (z/x'))

phi x z d =
  let x' = if x == 0 then 10**(-10) else x in
  atan (norm x z / d) * sin (atan (z/x'))

-- test theta theta'
test f g = and [cond f g x z d| (x,z,d) <- take 100 randos3]
  where
    cond f g x z d = abs (f x z d - g x z d) < 10**(-14)

randos3 = [ (x, z, d) | (d, (x, z)) <- zip randos10 $ zip randosI randosJ]
  where
    randosJ  = randomRs (0, 1::Double) $ mkStdGen 11
    randosI  = randomRs (0, 1::Double) $ mkStdGen 16
    randos10 = randomRs (0,10::Double) $ mkStdGen 23