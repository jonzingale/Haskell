module RayTracer.Crossings where

type Coords = (XCoord, YCoord, ZCoord)
type EntryCoords = (XCoord, ZCoord)
type EntryAngles = (Angle, Angle)
type XCoord = Double
type YCoord = Double
type ZCoord = Double
type Angle  = Double

{--
rewrite using U.Vector?
https://stackoverflow.com/questions/3276240/tools-for-analyzing-performance-of-a-haskell-program/3276557#3276557
http://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector-Unboxed.html

U.enumFrom 1 top
--}

xcrossings :: EntryCoords -> EntryAngles -> [Coords]
xcrossings (x, z) (θ, φ)
  | θ > pi/2 = [(cc x - k,
                 0 - (k + frac_offset x) * tan θ,
                 z - (k + frac_offset x) / (tan φ * cos θ)) | k <- [1..]]

  | otherwise = [(ff x + k,
                  0 + (k - frac x) * tan θ,
                  z + (k - frac x) / (tan φ * cos θ)) | k <- [1..]]

ycrossings :: EntryCoords -> EntryAngles -> [Coords]
ycrossings (x, z) (θ, φ) =
  [(x + k / tan θ, k, z + k / (tan φ * sin θ)) | k <- [1..]]

zcrossings :: EntryCoords -> EntryAngles -> [Coords]
zcrossings (x, z) (θ, φ)
  | φ > pi/2 = [(x - (k + frac_offset z) * cos θ * tan φ,
                 0 - (k + frac_offset z) * sin θ * tan φ,
                 cc z - k) | k <- [1..]]

  | otherwise = [(x + (k - frac z) * cos θ * tan φ,
                  0 + (k - frac z) * sin θ * tan φ,
                  ff z + k) | k <- [1..]]

cc, ff, frac, frac_offset :: Double -> Double
cc = fromIntegral.ceiling
ff = fromIntegral.floor
frac = snd.properFraction

frac_offset z | frac z == 0 = 0
              | otherwise = frac z - 1