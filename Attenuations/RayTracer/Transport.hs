module RayTracer.Transport where
import RayTracer.RayLength
import Data.Array

arraySize = 5
array = [[1..5], [6..11], [15..20], [21..26], [31..36]]

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
-- lookUp :: [[Double]] -> (Double, Double) -> Double
-- lookUp as (ii, jj) = let (i, j) = (floor ii, floor jj) in (as!!i)!!j

-- StraightFoward calculation
xks x theta s = [ x + k / tan theta | k <- [0..s]]
yks y theta s = [ y + k * tan theta | k <- [0..s]]

fractional :: Double -> Double
fractional = snd.properFraction

-- A real test will be that partials sum to the same as any total.
lim = 31622 -- (10^9)**(1/3) ~1/5 second.
theWalk theta = walk (xks 0 theta lim) (yks 0.25 theta lim) theta
  where
    walk _ [] _ = 0
    walk [] _ _ = 0
    walk (x:xs) (y:ys) theta = case x < y of
      True ->
        rayLength (fractional x, 0) theta + walk xs (y:ys) theta
      False ->
        rayLength (0, fractional y) theta + walk (x:xs) ys theta










