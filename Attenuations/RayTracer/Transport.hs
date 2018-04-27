module RayTracer.Transport where
import RayTracer.FileToVector
import RayTracer.RayLength
import Data.Array

arraySize = 5
myArray :: [[Double]]
myArray = [[1..5], [6..11], [15..20], [21..26], [31..36]]

{--
rayLength needs to change. How can I be sure that I am
passing legitimate values (x,0) or (y,0). Further,
calculating tan θ once is all that is necessary and
would greatly simplify the code.
--}

-- transport cs θ = f cs 0 0 θ 0
--   where
--     retCond x y j k t = x + j*tan t >= arraySize || y + k*tan t >= arraySize

--     g (a,b) (r,s) th = lookUp array (r,s) * rayLength (a,b) th -- see note above

--     f (x, y) j k θ accum | retCond x y j k θ = accum
--                          | x + j*tan θ <= y + k*tan θ =
--                             f (x,y) (j+1) k θ (accum + g (x,y) (j,k) θ)
--                          | otherwise =
--                             f (x,y) j (k+1) θ (accum + g (x,y) (j,k) θ)

-- Eventually this will be either Array
-- or Accelerate and likely not [[a]] but [a].
lookUp :: [[Double]] -> (Double, Double) -> Double
lookUp as (ii, jj) = let (i, j) = (floor ii, floor jj) in (as!!i)!!j

-- StraightFoward calculation
xks x theta s = [ x + k / tan theta | k <- [0..s]]
yks y theta s = [ y + k * tan theta | k <- [0..s]]

fractional :: Double -> Double
fractional = snd.properFraction

-- A real test will be that partials sum to the same as any total.
-- like totalAttenuation without the array.
lim = 1000 * sqrt(3/2) - 1 -- longest length in cube as diagonal in square (secs 0.01)

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

-- THE Naive approach 
-- for array size 5
-- totalAttenuation (pi/4)
totalAttenuation theta =
  let xs x theta s = [ properFraction $ x + k / tan theta | k <- [0..s]] in
  let ys y theta s = [ properFraction $ y + k * tan theta | k <- [0..s]] in
  let xwalk = (xs 0 theta 4) in
  let ywalk = (ys 0.25 theta 4) in

  walk xwalk ywalk theta

  where
    walk _ [] _ = 0
    walk [] _ _ = 0
    walk ((x,s):xs) ((y,t):ys) theta =
      let attVal (s,t) = lookUp myArray (s, t) in
      case x < y of
      True ->
        attVal (s,t) * rayLength (fractional s, 0) theta + walk xs ((y,t):ys) theta
      False ->
        attVal (s,t) * rayLength (0, fractional t) theta + walk ((x,s):xs) ys theta






