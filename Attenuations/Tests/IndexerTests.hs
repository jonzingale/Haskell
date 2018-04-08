
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.IndexerTests where
import RayTracer.RayLength
import RayTracer.Rhythm
import Test.Framework

-- toAngleDeg
prop_radiansToDeg :: Slope -> Bool
prop_radiansToDeg (n,d) =
  toAngleDeg (n,d) == toAngleRad (n,d) * 180 / pi

--- Index Generator Tests.
test_rabbits :: IO ()
test_rabbits = do
  let rhythm = take 15 $ rabbits (5,3)
  assertEqual rhythm ".rLrrLr.rLrrLr."
