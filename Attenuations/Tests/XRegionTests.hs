
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.XRegionTests where
import Tests.ExplicitGenerators
import RayTracer.RayLength
import Test.Framework

{--

 εδ γ βα   μ ρκ     κ'ρ'μ'
η_\\|//_η  |//_ι  ι'_\\|

--}

-- x - Boundary Tests
prop_nean, prop_bgd :: Gen Bool

prop_nean = do -- don't need eta.
  x <- interval
  let regions = [alpha, eta, epsilon]
  let flat = regions <*> [(x,0)] <*> [pi]
  return $ [1,1,1] == flat

prop_bgd = do -- don't need gamma.
  x <- interval
  let regions = [beta, gamma, delta]
  let pi2s = regions <*> [(x,0)] <*> [pi/2]
  return $ [1,1,1] == pi2s

prop_edBoundary, prop_abBoundary :: Bool

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
prop_alphaIsReflectedEpsilon :: Gen Bool
prop_alphaIsReflectedEpsilon = do
  x <- interval
  theta <- alphaRegion x
  let tolAlpha = tol.alpha (x,0) $ theta
  let reflect = tol.(uncurry epsilon).reflectY
  let tolEpsil = reflect ((x,0), theta)
  return $ tolAlpha == tolEpsil

prop_betaIsReflectedDelta :: Gen Bool
prop_betaIsReflectedDelta = do
  x <- interval
  theta <- betaRegion x
  let tolBeta = tol.beta (x,0) $ theta
  let reflect = tol.(uncurry delta).reflectY
  let tolDelt = reflect ((x,0), theta)
  return $ tolBeta == tolDelt

-- prop_RotRhoIsEps :: Gen Bool
-- prop_RotRhoIsEps = do
--   x  <- interval
--   th <- epsilonRegion x
--   let eps = epsilon (x,0) th
--   let rrh = (uncurry rho).rot270 $ ((x,0), th)
--   let atol d = round $ d * 10^13 -- fairly stable!
--   return $ atol eps == atol rrh

prop_RotKapIsDel :: Gen Bool
prop_RotKapIsDel = do
  x  <- interval
  th <- deltaRegion $ x
  let del = delta (x,0) th
  let rka = (uncurry kappa).rot270 $ ((x,0), th)
  let atol d = round $ d * 10^13 -- fairly stable!
  return $ atol del == atol rka

