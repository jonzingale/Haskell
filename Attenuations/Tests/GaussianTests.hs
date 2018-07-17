{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.GaussianTests where
import Tests.ExplicitGenerators
import RayTracer.GaussianBeam
import Test.Framework

diag = sqrt 2 / 2

{--
changing the ray by pi/2 one way or the other
seems to break either the one test or the other.
--}

-- Given a unit cone, diagonals are what I expect.
prop_DiagonalAngles :: Gen Bool
prop_DiagonalAngles = do
  d <- distance
  s <- sign
  r <- sign
  let (θ, φ) = snd.ray d $ (s * d * diag, r * d * diag)
  return $ eBalls 10 (θ, φ) (s * atan diag, r * atan diag)

-- As d -> 0 all angles tend toward 0 or pi.
prop_AngleSpraysAway :: TestCoords -> Gen Bool
prop_AngleSpraysAway (Coords (x, z)) = do
  let d = 0
  let (θ, φ) = snd.ray d $ (x*1000, z*1000)
  return $ eBall 13 0 (abs θ) || eBall 13 pi (abs θ) &&
           eBall 13 0 (abs φ) || eBall 13 pi (abs φ)

prop_AnglesTendsToParallel :: TestCoords -> Gen Bool
prop_AnglesTendsToParallel (Coords (x, z)) = do
  let d = 10**13
  let (θ, φ) = snd.ray d $ (x*1000, z*1000)
  return $ eBalls 10 (abs θ, abs φ) (pi/2, pi/2)
