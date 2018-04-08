
-- http://hackage.haskell.org/package/HTF-0.13.2.4/docs/Test-Framework-Tutorial.html
-- http://www.pstcc.edu/departments/natural_behavioral_sciences/Web%20Physics/TRIGG/Chapter(4).htm
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AttenuationTest where
import RayTracer.RayLength
import RayTracer.Lattice
import RayTracer.Rhythm
import Test.Framework

import System.Random
{--
TODO:
Specify ranges of validity for prop tests.
--}

--- Attenuation Tests.
{--
 εδ γ βα
η_\\|//_η

eta (x,0) pi = 1
epsilon (x,0) theta = -x / cos theta
delta (x,0) theta = 1 / cos theta
gamma (x,0) (pi/2) = 1 
beta  (x,0) theta = 1 / sin theta
alpha (x,0) theta = (1-x) / cos theta
eta' (x,0) 0  = 1
--}

--tolerance 12 decimal places
tol :: Double -> Integer
tol d = round $ d * 10^12

-- boundary tests
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

-- reflection tests
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

--- RayTracer Tests.
test_rabbits :: IO ()
test_rabbits = do
  let rhythm = take 15 $ rabbits (5,3)
  assertEqual rhythm ".rLrrLr.rLrrLr."



main = htfMain htf_thisModulesTests