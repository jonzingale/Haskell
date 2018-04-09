
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.SymmetryTests where
import RayTracer.RayLength
import Test.Framework

--tolerance 12 decimal places
--better would be an epsilon ball
type DoubleCoords =  ((Double, Double), Double)
type IntegerCoords = ((Integer, Integer), Integer)

tau = 2 * pi

tol :: Double -> Integer
tol d = round $ d * 10^12

mtol :: DoubleCoords -> IntegerCoords
mtol ((x,y), t) = ((tol x, tol y), tol t)

-- Rotation Tests
prop_rot90 = do
  θ <- choose (0, pi)
  return $ (tol.cos) (pi/2 - θ) == (tol.sin) θ

prop_rot4Id x y = do
  t <- choose (0, tau)
  return $ (mtol.diffPi.rotFour) ((x, y),t) == mtol ((x, y),t)
  where
    diffPi ((x, y),theta) = ((x, y), theta - tau)
    rotFour = foldr (.) id [rot90 | x<-[1..4]]

prop_rotInv' :: (Point, Angle) -> Bool
prop_rotInv' cs = (mtol.rot90.rot270) cs == mtol cs

prop_rotInv :: (Point, Angle) -> Bool
prop_rotInv cs = (mtol.rot270.rot90) cs == mtol cs
