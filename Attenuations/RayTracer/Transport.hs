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
--}

(x,y,theta,size) = (8/9, 0, pi/3,5.0)
xs = [ x + k / tan theta | k <- [0..size-1.0]]
ys = [ (k + 1 - fractional x) * tan theta | k <- [0..size-1.0]]
testRayLength = totalRayLength (8/9) (pi/3) 1

fractional :: Double -> Double
fractional = snd.properFraction

totalRayLength x theta size =
  let xcrossings = [ x + k / tan theta | k <- [0..size-1]] in -- x @ yn
  let ycrossings = [ (k + 1 - fractional x) * tan theta | k <- [0..size-1]] in -- y @ xn
  rayLength (fractional x,0) theta + walk xcrossings ycrossings theta -- the sum because

  where
    walk _ [] _ = 0
    walk [] _ _ = 0
    walk (x:xs) (y:ys) theta = case x < y of
      True ->
        rayLength (fractional x, 0) theta + walk xs (y:ys) theta
      False ->
        rayLength (0, fractional y) theta + walk (x:xs) ys theta

main = do
  myArray <- anArray
  return $ totalRayLength (8/9) (pi/3) 100
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

