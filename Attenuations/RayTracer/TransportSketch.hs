module RayTracer.TransportSketch (totalRayLength) where
import RayTracer.FileToVector
import RayTracer.RayLength

arraySize = 4
(x, theta, phi) = (8/9, pi/3, pi/2)
testRayLength = totalRayLength (8/9) (pi/3)

yval = (1-fractional x) * tan theta
ycrossings = takeWhile ((< arraySize).norm) [ (k, yval + k * tan theta) | k <- [0..]]
xcrossings = takeWhile ((< arraySize).norm) [ (x + k / tan theta, k) | k <- [0..]]

norm :: (Double, Double) -> Double
norm (x,y) = sqrt $ x*x + y*y

fractional :: Double -> Double
fractional = snd.properFraction

walkD _ [] _ = []
walkD [] _ _ = []
walkD (x:xs) (y:ys) theta
  | theta == pi/2 || theta == 0 = [] -- to cover asymptotics
  | x > arraySize || y > arraySize = []
  | x < y = x : walkD xs (y:ys) theta
  | otherwise = y : walkD (x:xs) ys theta

totalRayLength x theta =
  let yval = (1 - fractional x) * tan theta in -- calculate 1 time.
  let ycrossings = [ yval + k * tan theta | k <- [0..]] in -- y @ xn
  let xcrossings = [ x + k / tan theta | k <- [0..]] in -- x @ yn
  walk xcrossings ycrossings theta

  where
    walk _ [] _ = 0
    walk [] _ _ = 0
    walk (x:xs) (y:ys) theta
      | theta == pi/2 || theta == 0 = arraySize -- to cover asymptotics
      | x > arraySize-1 || y > arraySize = 0
      | x < y = rayLength (fractional x, 0) theta + walk xs (y:ys) theta
      | otherwise = rayLength (0, fractional y) theta + walk (x:xs) ys theta


main = do
  myArray <- anArray
  return $ totalRayLength x theta
