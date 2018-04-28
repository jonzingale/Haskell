module RayTracer.Transport (totalRayLength, totalAttenuation) where
import RayTracer.FileToVector
import RayTracer.RayLength

{--
rayLength needs to change. How can I be sure that I am
passing legitimate values (x,0) or (y,0). Further,
calculating tan θ once is all that is necessary and
would greatly simplify the code.

A real test will be that partials sum to the same as any total.
--}
fractional :: Double -> Double
fractional = snd.properFraction

totalRayLength theta =
  let ywalk = yks 0.25 theta 999 in
  let xwalk = xks 0 theta 999 in

  walk xwalk ywalk theta

  where
    xks x theta s = [ x + k / tan theta | k <- [0..s]]
    yks y theta s = [ y + k * tan theta | k <- [0..s]]
    walk _ [] _ = 0
    walk [] _ _ = 0
    walk (x:xs) (y:ys) theta = case x < y of
      True ->
        rayLength (fractional x, 0) theta + walk xs (y:ys) theta
      False ->
        rayLength (0, fractional y) theta + walk (x:xs) ys theta

totalAttenuation (x,y) theta ary =
  let xcrossings = [ x + k / tan theta | k <- [0..999]] in
  let ycrossings = [ y + k * tan theta | k <- [0..999]] in

  walk xcrossings ycrossings theta ary

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


main = do
  myArray <- anArray
  return $ totalAttenuation (0,123.1) (pi/2) myArray
