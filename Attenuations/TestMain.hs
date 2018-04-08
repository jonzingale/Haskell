
-- http://hackage.haskell.org/package/HTF-0.13.2.4/docs/Test-Framework-Tutorial.html
-- http://www.pstcc.edu/departments/natural_behavioral_sciences/Web%20Physics/TRIGG/Chapter(4).htm
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AttenuationTest where
import RayTracer.RayLength
import RayTracer.Lattice
import RayTracer.Rhythm
import Test.Framework
{--
TODO:
* simplify x types
* find general prop testing
* rotation mapping x - y
* simplify y types
--}

--- Attenuation Tests.

--tolerance 12 decimal places
tol :: Double -> Integer
tol d = round $ d * 10^12

-- Rotation Tests
prop_rot90 = do
  x <- choose (0, pi)
  return $ (tol.cos) (pi/2 - x) == (tol.sin) x

-- prop_rotInv :: Double -> Angle -> Bool -- see AssocComp example
-- prop_rotInv y t = (rot90.rot270) ((0, y),t) == ((0,y),t)

prop_rotInv = do
  x <- choose (0,1)
  y <- choose (0,1)
  t <- choose (0,2*pi)
  let mtol ((x,y), t) = ((tol x, tol y), tol t)
  let rotTol = (mtol.rot90.rot270) ((x, y),t)
  return $ rotTol == mtol ((x,y), t)

{--
x - Boundary Tests

 εδ γ βα
η_\\|//_η
--}

prop_nean = do -- don't need eta.
  x <- choose (0::Double, 1)
  let regions = [alpha, eta, epsilon]
  let flat = regions <*> [(x,0)] <*> [pi]
  return $ [1,1,1] == flat

prop_bgd = do -- don't need gamma.
  x <- choose (0::Double, 1)
  let regions = [beta, gamma, delta]
  let pi2s = regions <*> [(x,0)] <*> [pi/2]
  return $ [1,1,1] == pi2s

prop_edBoundary = -- epsilon - delta boundary
  let params = (,) (1,0) (3*pi/4) in
  let regions = uncurry <$> [epsilon, delta] in
  let eqF x = (tol.sqrt) 2 == tol x in
  all eqF $ regions <*> [params]

prop_abBoundary = -- alpha - beta boundary
  let params = (,) (0,0) (pi/4) in
  let regions = uncurry <$> [alpha, beta] in
  let eqF x = (tol.sqrt) 2 == tol x in
  all eqF $ regions <*> [params]

-- x - Reflection Tests
prop_alphaIsReflectedEpsilon = do
  let abThresh t = pi/4 + pi*t/4
  x <- choose (0, 1)
  theta <- choose (0, abThresh x)
  let tolAlpha = tol.alpha (x,0) $ theta
  let tolEpsil = tol.epsilon (1-x,0) $ pi - theta
  return $ tolAlpha == tolEpsil

prop_betaIsReflectedDelta = do
  let abThresh t = pi/4 + pi*t/4
  x <- choose (0, 1)
  theta <- choose (abThresh x, pi/2)
  let tolBeta = tol.beta (x,0) $ theta
  let tolDelt = tol.delta (1-x,0) $ pi - theta
  return $ tolBeta == tolDelt

-- toAngleDeg
prop_radiansToDeg :: Slope -> Bool
prop_radiansToDeg (n,d) =
  toAngleDeg (n,d) == toAngleRad (n,d) * 180 / pi

--- Index Generator Tests.
test_rabbits :: IO ()
test_rabbits = do
  let rhythm = take 15 $ rabbits (5,3)
  assertEqual rhythm ".rLrrLr.rLrrLr."



main = htfMain htf_thisModulesTests