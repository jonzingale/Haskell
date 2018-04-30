module RayTracer.TransportSketch where
import RayTracer.FileToVector
import RayTracer.RayLength

{--
NOTES:
 * totalAttenuation will always be initialized such that (x,0).
 * tan only really works here because I am considering θ <= pi/2. NEEDS GENERALIZED.
 * There WILL be double counting if the points are on the diagonal.
--}
arraySize = 4
(x,theta) = (8/9, pi/3)

yCxs x theta =
  let yval = (1-fractional x) * tan theta in
  f yval theta 0 arraySize
  where
    f yy theta k s | norm (k, yy + k * tan theta) > s = []
                   | otherwise = yy + k * tan theta : f yy theta (k+1) s

xCys x theta = f x theta 0 arraySize
  where
    f b theta k s | norm (b + k / tan theta, k) > s = []
                  | otherwise = b + k / tan theta : f b theta (k+1) s


{--
The norm is not the right idea and so this will need to be rewritten.
Really, YCxs :: [(y, raylen)] where raylen comes from equation for the line.
0 = (tan θ)*t + x. Letting x vary and solving for t should give lengths.
perhaps, y = (tan θ)*t + x, but I will wishful think not for now.

Even better is realizing that the region detector already
determines in which cell the next calculation will occur.
--}
norm :: (Double, Double) -> Double
norm (x,y) = sqrt $ x*x + y*y

fractional :: Double -> Double
fractional = snd.properFraction

totalRayLength x theta =
  let ycrossings = yCxs x theta in
  let xcrossings = xCys x theta in
  walk xcrossings ycrossings theta

  where
    walk _ [] _ = 0
    walk [] _ _ = 0
    walk (x:xs) (y:ys) theta
      | theta == pi/2 || theta == 0 = arraySize -- to cover asymptotics
      -- | i >= arraySize || j >= arraySize = 0 -- lattice boundary
      | x <= y = rayLength (fractional x, 0) theta + walk xs (y:ys) theta
      | otherwise = rayLength (0, fractional y) theta + walk (x:xs) ys theta

main = do
  myArray <- anArray
  return $ totalRayLength x theta

walkD _ [] _ = []
walkD [] _ _ = []
walkD (x:xs) (y:ys) theta
  | theta == pi/2 || theta == 0 = [] -- to cover asymptotics
  | x > arraySize || y > arraySize = []
  | x < y = x : walkD xs (y:ys) theta
  | otherwise = y : walkD (x:xs) ys theta