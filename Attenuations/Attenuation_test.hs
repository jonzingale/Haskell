
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AttenuationTest where
import Test.Framework
import Attenuation


-- rayLength
test_rayLength_equality = do 
  let (point, slope) = ((5/14, 0), (14,19))
  let testRL  = rayLength  point (toAngleRad slope)
  let testRL' = rayLength' point slope
  assertEqual testRL testRL'

-- toAngleDeg
prop_toAngleDeg :: Slope -> Bool
prop_toAngleDeg (n,d) = toAngleDeg (n,d) == toAngleRad (n,d) * 180 / pi

main = htfMain htf_thisModulesTests