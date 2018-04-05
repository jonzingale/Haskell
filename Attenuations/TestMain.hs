
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AttenuationTest where
import RayTracer.Attenuation
import Test.Framework


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

main = htfMain htf_thisModulesTests