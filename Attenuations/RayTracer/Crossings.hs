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
xcrossings :: EntryCoords -> EntryAngles -> [Coords]
xcrossings (x, z) (θ, φ)
  | θ > pi/2 = [(ff x - k, -(frac x + k)*tan θ, zc z θ φ k) | k<-[0..]]
  | otherwise = [(ff x + k + 1, (1 - frac x + k)*tan θ, zc z θ φ k) | k<-[0..]]
  where
    zc z θ φ k | φ == 0 || φ == pi = z -- probably should get theta case too.
               | φ <= pi/2 = z + k / (tan φ * cos θ)
               | otherwise = z + k / (tan φ * cos θ) -- not sure here

ycrossings :: EntryCoords -> EntryAngles -> [Coords]
ycrossings (x, z) (θ, φ) = [ (x + k / tan θ, k, zc z θ φ k) | k <- [1..]]
  where
    zc z θ φ k | φ == 0 || φ == pi = z -- probably should get theta case too.
               | φ < pi/2  = z + k / (tan φ * sin θ)
               | otherwise = z - k / (tan φ * sin θ) -- not sure here

-- verify how x- and y- components may need initial values.
-- should be all 1s at cheapZs (0,0) (pi/4, pi/4)
zcrossings :: EntryCoords -> EntryAngles -> [Coords]
zcrossings (x, z) (θ, φ)
  | φ > pi/2 = [(x + k * cos θ * tan φ  / root2Over2,
                 k * sin θ * tan φ  / root2Over2,
                 cc z - k) | k <- [1..]]  -- not sure here

  | otherwise = [(x + k * cos θ * tan φ  / root2Over2,
                  k * sin θ * tan φ  / root2Over2,
                  ff z + k) | k <- [1..]] -- not sure here


cc, ff :: Double -> Double
cc = fromIntegral.ceiling
ff = fromIntegral.floor
frac = snd.properFraction
root2Over2 = sqrt 2 / 2