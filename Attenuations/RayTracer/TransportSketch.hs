module RayTracer.TransportSketch (totalRayLength) where
import RayTracer.FileToVector
import RayTracer.RayLength

arraySize = 6
(x, theta, phi) = (8/9, pi/3, pi/2)
testRayLength = totalRayLength (8/9) (pi/3)

-- These are the y-coordinates when the ray passes integer valued xs.
-- (raylength thus far, next y value at x crossing)
xcrossings = takeWhile ((< arraySize).fst) [ (rayLen x theta k, yval + k * tan theta) | k <- [1..]]
  where
    rayLen x t k = (1 - fractional x + k) * 1/ cos t
    yval = (1-fractional x) * tan theta

{--
1.1547005383792517 = 0.9324783161570293 + 0.22222222222
-- ycrossings = [(0.8888888888888888,0.0),
                 (1.4662391580785148,1.1547005383792517),
                 (2.0435894272681407,2.3094010767585034),
                 (2.620939696457767,3.464101615137755)]
-- steps toward explict ray extensions. returns the pair
-- (next x value at y crossing, raylength thus far).
--}
-- These are the x-coordinates when the ray passes integer valued ys.
ycrossings = normWhile [ (x + k / tan theta, rayLen x theta k) | k <- [0..]]
  where
    rayLen x t k = sqrt $ k**2 * (1 + 1/(tan t)**2)
    normWhile = takeWhile ((< arraySize).snd)


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
