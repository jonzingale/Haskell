
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
tol d = round $ d * 10^11

mtol :: DoubleCoords -> IntegerCoords
mtol ((x,y), t) = ((tol x, tol y), tol t)

-- Rotation Tests
prop_shift90 :: Gen Bool
prop_shift90 = do
  θ <- choose (0, pi)
  return $ (tol.cos) (pi/2 - θ) == (tol.sin) θ

prop_rot4Id :: Point -> Gen Bool
prop_rot4Id cs = do
  t <- choose (0, tau)
  return $ (mtol.diffPi.rotFour) (cs, t) == mtol (cs, t)
  where
    diffPi ((x, y),theta) = ((x, y), theta - tau)
    rotFour = foldr (.) id [rot90 | x<-[1..4]]

prop_rotInv' :: (Point, Angle) -> Bool
prop_rotInv' cs = (mtol.rot90.rot270) cs == mtol cs

prop_rotInv :: (Point, Angle) -> Bool
prop_rotInv cs = (mtol.rot270.rot90) cs == mtol cs

prop_reflectInv :: (Point, Angle) -> Bool
prop_reflectInv cs = (mtol.reflectY.reflectY) cs == mtol cs

{--
 εδ γ βα   μ ρκ
η_\\|//_η  |//_ι
--}

prop_RotRhoIsEps :: Gen Bool
prop_RotRhoIsEps = do
  let atol d = round $ d * 10^13 -- fairly stable!
  x  <- choose (0, 1::Double)
  th <- choose (3*pi/4 + x*pi/4, pi) -- super-εδ-Condition
  let eps = epsilon (x,0) th
  let rrh = uncurry rho $ rot270 ((x,0), th)
  return $ atol eps == atol rrh

prop_RotKapIsDel :: Gen Bool
prop_RotKapIsDel = do
  let atol d = round $ d * 10^13
  x  <- choose (0, 1::Double)
  th <- choose (pi/2, pi/2 + x*pi/4) -- sub-εδ-Condition
  let del = delta (x,0) th
  let rka = uncurry kappa $ rot270 ((x,0), th)
  return $ atol del == atol rka

-- (theta, x) = (pi*7/8, 0.75) -- epsilon coords
-- rotEp = rot270 ((x,0),theta) -- == ((0.0,0.25),1.1780972450961724)

