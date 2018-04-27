module RayTracer.Transport where
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

-- StraightFoward calculation
xks x theta s = [ x + k / tan theta | k <- [0..s]]
yks y theta s = [ y + k * tan theta | k <- [0..s]]

-- longest length in cube as diagonal
-- in square (secs 0.01)
lim = 1000 * sqrt(3/2) - 1 

totalRayLength theta =
  let xwalk = (xks 0 theta lim) in
  let ywalk = (yks 0.25 theta lim) in
  walk xwalk ywalk theta

  where
    walk _ [] _ = 0
    walk [] _ _ = 0
    walk (x:xs) (y:ys) theta = case x < y of
      True ->
        rayLength (fractional x, 0) theta + walk xs (y:ys) theta
      False ->
        rayLength (0, fractional y) theta + walk (x:xs) ys theta

totalAttenuation theta ary =
  let xs x theta s = [ properFraction $ x + k / tan theta | k <- [0..s]] in
  let ys y theta s = [ properFraction $ y + k * tan theta | k <- [0..s]] in
  let xwalk = (xs 0 theta 4) in
  let ywalk = (ys 0.25 theta 4) in

  walk xwalk ywalk theta ary

  where
    walk _ [] _ _ = 0
    walk [] _ _ _ = 0
    walk ((x,s):xs) ((y,t):ys) theta ary =
      let val = qArray (floor s, floor t) ary in
      case x < y of
      True ->
        val * rayLength (fractional s, 0) theta + walk xs ((y,t):ys) theta ary
      False ->
        val * rayLength (0, fractional t) theta + walk ((x,s):xs) ys theta ary


main = do
  myArray <- anArray
  return $ totalAttenuation (pi/2) myArray


