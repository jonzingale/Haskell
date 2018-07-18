{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.GaussianTests where
import Tests.ExplicitGenerators
import RayTracer.GaussianBeam
import Test.Framework

-- Given a unit cone, diagonals are what I expect.
prop_NormalDiagonals :: TestDistance -> TestSignPair -> Gen Bool
prop_NormalDiagonals (Distance d) (Sigs (s, r)) = do
  let (θ, φ) = snd.ray d $ (s * d * dd, r * d * dd)
  return $ eBalls 10 (θ, φ) (rad s, rad r)
  where
    dd = sqrt 2 / 2
    rad s | s == 1 = atan (1 / dd)
          | otherwise = pi - atan (1 / dd)

-- X and Z components in equal parts give 45s.
prop_EqualComponents :: TestDistance -> TestSignPair -> Gen Bool
prop_EqualComponents (Distance d) (Sigs (s, r)) = do
  let (θ, φ) = snd.ray d $ (s * d, r * d)
  return $ eBalls 10 (θ, φ) (rad s, rad r)
  where
    rad s | s == 1 = pi/4
          | otherwise = 3*pi/4

-- As d -> 0 all angles tend toward 0 or pi.
prop_AngleSpraysAway :: TestCoords -> TestSignPair -> Gen Bool
prop_AngleSpraysAway (Coords (x, z)) (Sigs (s, r)) = do
  let (θ, φ) = snd.ray 0 $ (s*x*1000, r*z*1000)
  return $ eBall 13 (rad s) θ && eBall 13 (rad r) φ
  where
    rad s | s == 1 = 0
          | otherwise = pi

-- As d -> ∞ all angles tend toward pi/2
prop_AnglesTendsToParallel :: TestCoords -> TestSignPair -> Gen Bool
prop_AnglesTendsToParallel (Coords (x, z)) (Sigs (s, r)) = do
  let (θ, φ) = snd.ray (10**13) $ (s*x*1000, r*z*1000)
  return $ eBalls 10 (abs θ, abs φ) (pi/2, pi/2)
