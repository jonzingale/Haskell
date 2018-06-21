module RayTracer.Crossings where

type Coords = (XCoord, YCoord, ZCoord)
type EntryCoords = (XCoord, ZCoord)
type EntryAngles = (Angle, Angle)
type XCoord = Double
type YCoord = Double
type ZCoord = Double
type Angle  = Double

{--
A start on incorporating the z and φ components.
The guess below cannot be correct. The z component
is a function of both φ and θ (a projective cone).
--}

-- correct for cheapXs (0,0) (pi/3, pi/4)
xcrossings :: EntryCoords -> EntryAngles -> [Coords]
xcrossings (x, z) (θ, φ)
  | θ > pi/2 = [(ff x - k, -(frac x + k) * tan θ, zc z θ φ k) | k<-[1..]]
  | otherwise = [(ff x + k, (k - frac x) * tan θ, zc z θ φ k) | k<-[1..]]
  where
    zc z θ φ k | φ <= pi/2 = z + k / (tan φ * cos θ)
               | otherwise = z + k / (tan φ * cos θ)

ycrossings :: EntryCoords -> EntryAngles -> [Coords]
ycrossings (x, z) (θ, φ) = [ (x + k / tan θ, k, zc z θ φ k) | k <- [1..]]
  where
    zc z θ φ k | φ < pi/2  = z + k / (tan φ * sin θ)
               | otherwise = z + k / (tan φ * sin θ)

-- verify how x- and y- components may need initial values.
-- CONSIDER: cheapZs (0,0.9) (atan 2, atan (sqrt 5))
zcrossings :: EntryCoords -> EntryAngles -> [Coords]
zcrossings (x, z) (θ, φ)
  | φ <= pi/2 = [(x + (k-frac z) * cos θ * tan φ, -- <- CHECK HERE.
                 (k-frac z) * sin θ * tan φ, -- <- CHECK HERE.
                 ff z + k) | k <- [1..]]

  | otherwise = [(x + k * cos θ * tan φ,
                  k * sin θ * tan φ,
                  ff z - k) | k <- [1..]]


cc, ff :: Double -> Double
cc = fromIntegral.ceiling
ff = fromIntegral.floor
frac = snd.properFraction
root2Over2 = sqrt 2 / 2