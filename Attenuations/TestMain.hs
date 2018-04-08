
-- http://hackage.haskell.org/package/HTF-0.13.2.4/docs/Test-Framework-Tutorial.html
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AttenuationTest where
import RayTracer.RayLength
import RayTracer.Lattice
import RayTracer.Rhythm
import Test.Framework

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
tol d = round $ d * 10^12

-- (x,0) region testing
prop_bgd = do -- beta - gamma - delta == pi/2
  x <- choose (0::Double,1)
  return $ (==) [1,1,1] $ [beta, gamma, delta] <*> [(x,0)] <*> [pi/2]

prop_ed = -- epsilon - delta == root2 at 3*pi/4 (1,0)
  let eps = tol.epsilon (1,0) $ 3*pi/4 in
  let del = tol.delta (1,0) $ 3*pi/4 in
  tol root2 == eps && eps == del

-- alpha is reflected epsilon
prop_alphaIsReflectedEpsilon = do
  let thetaCond t = pi/4 + pi*t/4
  x <- choose (0::Double, 1)
  theta <- choose (0::Double, thetaCond x)
  return $ (tol.alpha (x,0) $ theta) == (tol.epsilon (1-x, 0) $ pi-theta)

-- toAngleDeg
prop_radiansToDeg :: Slope -> Bool
prop_radiansToDeg (n,d) = toAngleDeg (n,d) == toAngleRad (n,d) * 180 / pi

--- RayTracer Tests.
test_rabbits :: IO ()
test_rabbits = do
  let rhythm = take 15 $ rabbits (5,3)
  assertEqual rhythm ".rLrrLr.rLrrLr."



main = htfMain htf_thisModulesTests