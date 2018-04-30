module RayTracer.Transport (totalRayLength, totalAttenuation) where
import RayTracer.FileToVector
import RayTracer.RayLength

{--
rayLength needs to change. How can I be sure that I am
passing legitimate values (x,0) or (y,0). Further,
calculating tan θ once is all that is necessary and
would greatly simplify the code.

A real test will be that partials sum to the same as any total.

NOTES:
 * totalAttenuation will always be initialized such that (x,0).
 * tan only really works here because I am considering θ <= pi/2. NEEDS GENERALIZED.
 * There WILL be double counting if the points are on the diagonal.
--}
arraySize = 4
testRayLength = totalRayLength (8/9) (pi/3)

(x, theta, phi) = (8/9, pi/3, pi/2)
yval = (1-fractional x) * tan theta
ycrossings = takeWhile ((< arraySize).norm) [ (k, yval + k * tan theta) | k <- [0..]]
xcrossings = takeWhile ((< arraySize).norm) [ (x + k / tan theta, k) | k <- [0..]]

norm :: (Double, Double) -> Double
norm (x,y) = sqrt $ x*x + y*y

walkD _ [] _ = []
walkD [] _ _ = []
walkD (x:xs) (y:ys) theta
  | theta == pi/2 || theta == 0 = [] -- to cover asymptotics
  | x > arraySize || y > arraySize = []
  | x < y = x : walkD xs (y:ys) theta
  | otherwise = y : walkD (x:xs) ys theta
  -- | x < y = rayLength (fractional x, 0) theta : walkD xs (y:ys) theta
  -- | otherwise = rayLength (0, fractional y) theta : walkD (x:xs) ys theta

-- (x,y,theta) = (8/9, 0, pi/3) -- 4.2222222222222205
-- (x,y,theta) = (0, 0, 1*pi/8) -- 6.308644059797899
-- (x,y,theta) = (0, 0, 3*pi/8) -- 6.308644059797899


fractional :: Double -> Double
fractional = snd.properFraction

totalRayLength x theta =
  let yval = (1 - fractional x) * tan theta in -- calculate 1 time.
  let ycrossings = [ yval + k * tan theta | k <- [0..]] in -- y @ xn
  let xcrossings = [ x + k / tan theta | k <- [0..]] in -- x @ yn
  walk xcrossings ycrossings theta

  where
    -- normWhile = takeWhile ((< arraySize).norm)
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
  -- return $ totalAttenuation (8/9) (pi/2) myArray -- still pretty buggy +x

totalAttenuation = undefined
{-- NEEDS HELP AFTER FIXING totalRayLength
-- totalAttenuation will always be initialized such that (x,0).
totalAttenuation x theta ary =
  -- array lengths should really be dependent on path lengths. These
  -- will be better written as a conditional on limit: x<1000 && y<1000.
  let xcrossings = [ x + k / tan theta | k <- [0..999]] in
  let ycrossings = [ y + k * tan theta | k <- [0..999]] in
  rayLength (fractional x,0) theta + walk xcrossings ycrossings theta ary

  where
    walk _ [] _ _ = 0
    walk [] _ _ _ = 0
    walk (x:xs) (y:ys) theta ary =
      let val = qArray (floor x, floor y) ary in
      case x < y of
      True -> -- bottom entry in frame
        val * rayLength (fractional x, 0) theta + walk xs (y:ys) theta ary
      False -> -- side entry in frame
        val * rayLength (0, fractional y) theta + walk (x:xs) ys theta ary
--}

