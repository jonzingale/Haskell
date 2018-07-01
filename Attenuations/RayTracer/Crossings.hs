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

-- correct for cheapXs (0,0) (pi/3, pi/4)
xcrossings :: EntryCoords -> EntryAngles -> [Coords]
xcrossings (x, z) (θ, φ)
  | φ == 0 || φ == pi || θ == pi/2 = infList
  | θ > pi/2 = [(cc x - k, -(frac x + k) * tan θ, zc z θ φ (-k) x) | k <- [1..]]
  | otherwise = [(ff x + k, (k - frac x) * tan θ, zc z θ φ k x) | k <- [1..]]
  where
    zc z θ φ k x | φ <= pi/2 = z + (k - frac x) / (tan φ * cos θ)
                 | otherwise = z + (k - frac x) / (tan φ * cos θ)

ycrossings :: EntryCoords -> EntryAngles -> [Coords]
ycrossings (x, z) (θ, φ)
  | φ == 0 || φ == pi || θ == 0 = infList
  | otherwise = [ (x + k / tan θ, k, zc z θ φ k) | k <- [1..]]
  where
    zc z θ φ k | φ < pi/2  = z + k / (tan φ * sin θ)
               | otherwise = z + k / (tan φ * sin θ)

zcrossings :: EntryCoords -> EntryAngles -> [Coords]
zcrossings (x, z) (θ, φ)
  | φ <= pi/2 = [(x + (k - frac z) * cos θ * tan φ,
                 (k - frac z) * sin θ * tan φ,
                 ff z + k) | k <- [1..]]

  | otherwise = [(x - (k + frac z) * cos θ * tan φ, -- This Guy needs settled.
                  -(frac z + k) * sin θ * tan φ,
                  cc z - k) | k <- [1..]]

  -- | otherwise = [(x + (k - frac z) * cos θ * tan φ, -- This Guy needs settled.
  --                 (k - frac z) * sin θ * tan φ,
  --                 ff z - k) | k <- [1..]]


cc, ff :: Double -> Double
cc = fromIntegral.ceiling
ff = fromIntegral.floor
frac = snd.properFraction