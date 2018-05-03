module RayTracer.TransportSketch  where
import RayTracer.FileToVector
import RayTracer.RayLength

fractional :: Double -> Double
fractional = snd.properFraction

arraySize = 6
(x, theta, phi) = (8/9, pi/3, pi/2)
-- testRayLength = totalRayLength (8/9) (pi/3)

-- ys vaues at integer x.
xcrossings x theta =
  let ypt k = x + k / tan theta in
  let rLen k = sqrt $ k**2 + (ypt k - x)**2 in
  [ (ypt k, rLen k) | k <- [1..]]

-- xs vaues at integer y.
ycrossings x theta = 
  let xpt k = (k-x) * tan theta in
  let rLen k = sqrt $ (k-x)**2 + (xpt k)**2 in
  [ (xpt k, rLen k) | k <- [1..]]

-- main = do
  -- myArray <- anArray
  -- return $ totalRayLength x theta
