
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.SymmetryTests where
import Tests.ExplicitGenerators
import RayTracer.RayLength
import Test.Framework

-- Rotation Tests
prop_shift90 :: Gen Bool
prop_shift90 = do
  θ <- zeroToPi
  return $ (tol.cos) (pi/2 - θ) == (tol.sin) θ

prop_rot4Id :: Point -> Gen Bool
prop_rot4Id cs = do
  t <- zeroToTau
  return $ (mtol.diffPi.rotFour) (cs, t) == mtol (cs, t)
  where
    diffPi ((x, y),theta) = ((x, y), theta - tau)
    rotFour = foldr (.) id [rot90 | x<-[1..4]]

prop_rotInv', prop_rotInv :: (Point, Angle) -> Bool
prop_rotInv' cs = (mtol.rot90.rot270) cs == mtol cs
prop_rotInv  cs = (mtol.rot270.rot90) cs == mtol cs

prop_reflectInv :: (Point, Angle) -> Bool
prop_reflectInv cs = (mtol.reflectY.reflectY) cs == mtol cs

