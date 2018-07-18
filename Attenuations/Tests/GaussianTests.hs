{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.GaussianTests where
import Tests.ExplicitGenerators
import RayTracer.GaussianBeam
import Test.Framework

-- Given a unit cone, diagonals are what I expect.
prop_NormalDiagonals :: TestDistance -> Gen Bool
prop_NormalDiagonals (Distance d) = do
  s <- sign
  r <- sign
  let (θ, φ) = snd.ray d $ (s * d * dd, r * d * dd)
  return $ eBalls 10 (θ, φ) (rad s, rad r)
  where
    dd = sqrt 2 / 2
    rad s | s == 1 = atan (1 / dd)
          | otherwise = pi - atan (1 / dd)

-- X and Z components in equal parts give 45s.
prop_EqualComponents :: TestDistance -> Gen Bool
prop_EqualComponents (Distance d) = do
  s <- sign
  r <- sign
  let (θ, φ) = snd.ray d $ (s * d, r * d)
  return $ eBalls 10 (θ, φ) (rad s, rad r)
  where
    rad s | s == 1 = pi/4
          | otherwise = 3*pi/4

-- As d -> 0 all angles tend toward 0 or pi.
prop_AngleSpraysAway :: TestCoords -> Gen Bool
prop_AngleSpraysAway (Coords (x, z)) = do
  let d = 0
  let (θ, φ) = snd.ray d $ (x*1000, z*1000)
  return $ eBall 13 0 (abs θ) || eBall 13 pi (abs θ) &&
           eBall 13 0 (abs φ) || eBall 13 pi (abs φ)

-- As d -> ∞ all angles tend toward pi/2
prop_AnglesTendsToParallel :: TestCoords -> Gen Bool
prop_AnglesTendsToParallel (Coords (x, z)) = do
  let d = 10**13
  let (θ, φ) = snd.ray d $ (x*1000, z*1000)
  return $ eBalls 10 (abs θ, abs φ) (pi/2, pi/2)
