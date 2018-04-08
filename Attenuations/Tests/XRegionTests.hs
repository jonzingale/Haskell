
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.XRegionTests where
import RayTracer.RayLength
import Test.Framework

--tolerance 12 decimal places
--better would be an epsilon ball
tol :: Double -> Integer
tol d = round $ d * 10^12

{--
 εδ γ βα
η_\\|//_η
--}

-- x - Boundary Tests
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
  let abThresh t = (1+t) * pi/4
  x <- choose (0, 1)
  theta <- choose (0, abThresh x)
  let tolAlpha = tol.alpha (x,0) $ theta
  let tolEpsil = tol.epsilon (1-x,0) $ pi - theta
  return $ tolAlpha == tolEpsil

prop_betaIsReflectedDelta = do
  let abThresh t = (1+t) * pi/4
  x <- choose (0, 1)
  theta <- choose (abThresh x, pi/2)
  let tolBeta = tol.beta (x,0) $ theta
  let tolDelt = tol.delta (1-x,0) $ pi - theta
  return $ tolBeta == tolDelt
