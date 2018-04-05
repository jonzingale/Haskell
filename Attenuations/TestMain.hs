
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AttenuationTest where
import RayTracer.RayLength
import RayTracer.Lattice
import RayTracer.Rhythm
import Test.Framework

--- Attenuation Tests.
-- rayLength
test_rayLengths_equality :: IO ()
test_rayLengths_equality = do 
  let (point, slope) = ((5/14, 0), (14,19))
  let testRL  = rayLength  point (toAngleRad slope)
  let testRL' = rayLength' point slope
  assertEqual testRL testRL'

-- toAngleDeg
prop_radiansToDeg :: Slope -> Bool
prop_radiansToDeg (n,d) = toAngleDeg (n,d) == toAngleRad (n,d) * 180 / pi

--- RayTracer Tests.
test_rabbits :: IO ()
test_rabbits = do
  let rhythm = take 15 $ rabbits (5,3)
  let rhythmData = ".rLrrLr.rLrrLr."
  assertEqual rhythm rhythmData



main = htfMain htf_thisModulesTests