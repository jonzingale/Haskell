module RayTracer.TransportSketch  where
import RayTracer.FileToVector
import RayTracer.RayLength

fractional :: Double -> Double
fractional = snd.properFraction

arraySize = 3.1 -- <--- another error prone spot.
-- (x, theta, phi) = (8/9, pi/3, pi/2)
-- testRayLength = totalRayLength (8/9) (pi/3)

-- length diffs
tourists = take 2 $ zip (xcrossings 0 (pi/4)) (ycrossings  0 (pi/4))

-- ys vaues at integer x.
xcrossings x theta =
  let ypt k = x + k / tan theta in
  let rLen k = sqrt $ k**2 + (ypt k - x)**2 in
  [ (ypt k, rLen k) | k <- [0..], ypt k >= 0]

-- xs vaues at integer y.
ycrossings x theta = 
  let xpt k = (k-x) * tan theta in
  let rLen k = sqrt $ (k-x)**2 + (xpt k)**2 in
  [ (xpt k, rLen k) | k <- [1..], xpt k >= 0]

--- close to being pretty good.
totalRayLength x theta =
  f (xcrossings x theta) (ycrossings x theta) arraySize 0
  where
    f ((y,r1):xcs) ((x,r2):ycs) s accum
      | y > s || x > s = accum
      | r1 < r2 = f xcs ((x,r2):ycs) s r1
      | otherwise = f ((y,r1):xcs) ycs s r2

---
ediff x y = floor x == floor y -- totalRayLength 0.1 (pi/4) ???

totalRayLengthA x theta =
  f (xcrossings x theta) (ycrossings x theta) [] arraySize
  where
    f ((y,r1):xcs) ((x,r2):ycs) accum s
      | r1 > s && r2 > s = accum
      | ediff x y = f xcs ycs (r1 : accum) s -- likely to be trouble
      | r1 < r2 = f xcs ((x,r2):ycs) (r1 : accum) s
      | otherwise = f ((y,r1):xcs) ycs (r2 : accum) s

-- main = do
  -- myArray <- anArray
  -- return $ totalRayLength x theta
