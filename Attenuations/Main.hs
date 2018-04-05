module Main where
import RayTracer.RayLength
import RayTracer.Lattice
import RayTracer.Rhythm

-- At some point create individual Test directory
-- and run tests through here somehow.

main = do
  let len = rayLength (2/5,0) (pi/4)
  print len