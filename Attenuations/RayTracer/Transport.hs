module RayTracer.Transport where
-- import RayTracer.RayLength

-- transport cs theta = f cs 0 0 theta 0
--   where
--     g (a,b) (r,s) th = lookUp array (r,s) * rayLength (a,b) th

--     f (x, y) j k theta accum | x + j*tan theta >= 1000 ||
--                                y + k*tan theta >= 1000 = accum
--                              | x + j*tan theta < y + k*tan theta =
--                                f (x,y) (j+1) k theta (g (x,y) (j,k) theta)

-- -- Eventually this will be either Array
-- -- or Accelerate and likely not [[a]] but [a].
-- lookUp :: [[a]] -> (Double, Double) -> Double
-- lookUp as (ii,jj) = let (i, j) = (floor ii, floor jj) in (as!!i)!!j