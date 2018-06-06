module RayTracer.Transport3D where
import RayTracer.FileToVector

type Coords = (XCoord, YCoord, ZCoord)
type EntryCoords = (XCoord, ZCoord)
type EntryAngles = (Angle, Angle)
type SegmentLength = Double
type XCoord = Double
type YCoord = Double
type ZCoord = Double
type Angle  = Double

-- displayTrace 2.3 (pi/5)
-- displayTrace x t = do
--   ary <- fileToAry "./Tests/data49Doubles"
--   let (_:ijSeg) = transport x t -- because the head is not necessary.
--   let eval = [ seg * (qArray 7 ij ary) | (ij, seg) <- takeWhile stopCond ijSeg]
--   putStr "evaluated total:\n\n"
--   putStr.unlines.(map show) $ eval
--   where
--     stopCond ((x,y), s) = x<=7 && y<=7

prettyYcrossings3D (x, s) (θ, φ) =
  let ycs = take 7 $ ycrossings' (x, s) (θ, φ) in
  putStr.unlines.(map show) $ ycs

{--
A start on incorporating the z and φ components.
The guess below cannot be correct. The z component
is a function of both φ and θ (a projective cone).
--}
ycrossings' :: EntryCoords -> EntryAngles -> [Coords]
ycrossings' (x, z) (θ, φ) = [ (x + k / tan θ, k, zc z φ θ k) | k <- [0..]]
  where
    zc z φ θ k | φ <= pi/2 = z + k / (tan φ * sin θ)
               | otherwise = z + k / (tan φ * sin θ) -- not sure here

xcrossings' :: EntryCoords -> EntryAngles -> [Coords]
xcrossings' (x, z) (θ, φ)
  | θ > pi/2 = [(ff x - k, -(frac x + k)*tan θ, zc z φ θ k) | k<-[0..]]
  | otherwise = [(ff x + k + 1, (1 - frac x + k)*tan θ, zc z φ θ k) | k<-[0..]]
  where
    zc z φ θ k | φ <= pi/2 = z + k / (tan φ * cos θ)
               | otherwise = z + k / (tan φ * cos θ) -- not sure here

-- verify how x- and y- components may need initial values.
zcrossings' :: EntryCoords -> EntryAngles -> [Coords]
zcrossings' (x, z) (θ, φ)
  | θ > pi/2 = [(k * cos θ * tan φ, k * sin θ * tan φ, ff z - k) | k<-[0..]]
  | otherwise = [(k * cos θ * tan φ, k * sin θ * tan φ, ff z + k + 1) | k<-[0..]] -- not sure here

-- this is going to need very very much work.
type IntCoords = (Int, Int, Int)
transport:: EntryCoords-> EntryAngles -> [(IntCoords, SegmentLength)]
transport (x, z) (θ, φ)
  | θ > pi/2 = f (xcs (x, z) (θ, φ))
                 (ycs (x, z) (θ, φ))
                 (zcs (x, z) (θ, φ))
                 (x, 0, z) -- pt
                 (ceiling x, -1, ceiling z) -- i j k
                 (negate 1) -- sig
  | otherwise = f (xcs (x, z) (θ, φ))
                  (ycs (x, z) (θ, φ))
                  (zcs (x, z) (θ, φ))
                  (x, 0, z)
                  (floor x, -1, floor z)
                  1
  where
    (xcs, ycs, zcs) = (xcrossings', ycrossings', zcrossings')

    -- xcs ycs zcs (p,q,r) (i,j,k) sig
    f ((xh,yh,zh): xcs) ((xv,yv,zv): ycs) ((xd,yd,zd): zcs) pt (i, j, k) sign
      | yh == minimum [yh, yv, yd] = -- x case
        ((i,j,k), segment pt (xh,yh,zh)) :
          f xcs ((xv,yv,zv): ycs) ((xd,yd,zd): zcs) (xh,yh,zh) (i+sign, j, k) sign

      | yv == minimum [yh, yv, yd] = -- y case
        ((i,j,k), segment pt (xv,yv,zv)) :
          f ((xh,yh,zh): xcs) ycs ((xd,yd,zd): zcs) (xv,yv,zv) (i, j+1, k) sign

      | yd == minimum [yh, yv, yd]  = -- z case
        ((i,j,k), segment pt (xv,yv,zv)) :
          f ((xh,yh,zh): xcs) ((xv,yv,zv): ycs) zcs (xv,yv,zv) (i, j, k+sign) sign

segment :: Coords -> Coords -> SegmentLength
segment (x1, y1, z1) (x2, y2, z2) =
  sqrt $ (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2

cc, ff :: Double -> Double
cc = fromIntegral.ceiling
ff = fromIntegral.floor
frac = snd.properFraction