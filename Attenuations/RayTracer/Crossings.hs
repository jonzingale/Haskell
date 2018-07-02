module RayTracer.Crossings where

type Coords = (XCoord, YCoord, ZCoord)
type EntryCoords = (XCoord, ZCoord)
type EntryAngles = (Angle, Angle)
type XCoord = Double
type YCoord = Double
type ZCoord = Double
type Angle  = Double

infinity = 10^10
infList = repeat (infinity, infinity, infinity)

xcrossings :: EntryCoords -> EntryAngles -> [Coords]
xcrossings (x, z) (θ, φ)
  | φ == 0 || φ == pi || θ == pi/2 = infList
  | θ > pi/2 = [(cc x - k, -(f x + k) * tan θ, z + zc θ φ k x) | k <- [1..]]
  | otherwise = [(ff x + k, (k - frac x) * tan θ, z + zc θ φ k x) | k <- [1..]]
  where
    f x | frac x == 0 = 0
        | otherwise = frac x - 1
    zc θ φ k x | θ >= pi/2 = -(k + f x) / (tan φ * cos θ)
               | otherwise = (k - frac x) / (tan φ * cos θ)

ycrossings :: EntryCoords -> EntryAngles -> [Coords]
ycrossings (x, z) (θ, φ)
  | φ == 0 || φ == pi || θ == 0 = infList
  | otherwise = [(x + k / tan θ, k, z + k / (tan φ * sin θ)) | k <- [1..]]

zcrossings :: EntryCoords -> EntryAngles -> [Coords]
zcrossings (x, z) (θ, φ)
  | φ <= pi/2 = [(x + (k - frac z) * cos θ * tan φ,
                 (k - frac z) * sin θ * tan φ,
                 ff z + k) | k <- [1..]]

  | otherwise = [(x - (k + f z) * cos θ * tan φ,
                  -(f z + k) * sin θ * tan φ,
                  cc z - k) | k <- [1..]]
  where
    f z | frac z == 0 = 0
        | otherwise = frac z - 1


cc, ff :: Double -> Double
cc = fromIntegral.ceiling
ff = fromIntegral.floor
frac = snd.properFraction