module Main where
import RayTracer.Attenuation

main = do
  let len = rayLength (2/5,0) (pi/4)
  print len