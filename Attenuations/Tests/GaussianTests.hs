{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.GaussianTests where
import Tests.ExplicitGenerators
import RayTracer.GaussianBeam
import Test.Framework

-- Given a unit cone, diagonals are what I expect.
prop_NormalDiagonals :: Center -> TestDistance -> TestSignPair -> Gen Bool
prop_NormalDiagonals c (Distance d) (Sigs (s, r)) = do
  let (θ, φ) = snd.ray d c $ (c + s * d * dd, c + r * d * dd)
  return $ eBalls 14 (θ, φ) (rad s, rad r)
  where
    dd = sqrt 2 / 2
    rad s | s == 1 = atan (1 / dd)
          | otherwise = pi - atan (1 / dd)

-- X and Z components in equal parts give 45s.
prop_EqualComponents :: Center -> TestDistance -> TestSignPair -> Gen Bool
prop_EqualComponents c (Distance d) (Sigs (s, r)) = do
  let (θ, φ) = snd.ray d c $ (c + s * d, c + r * d)
  return $ eBalls 14 (θ, φ) (rad s, rad r)
  where
    rad s | s == 1 = pi/4
          | otherwise = 3*pi/4

-- As d -> 0 all angles tend toward 0 or pi, unless exactly on center.
prop_AngleSpraysAway :: Center -> TestCoords -> TestSignPair -> Gen Bool
prop_AngleSpraysAway c (Coords (x, z)) (Sigs (s, r)) = do
  let (θ, φ) = snd.ray 0 c $ (c + s*x*100, c + r*z*100)
  return $ c == 0 || eBall 14 (rad s) θ && eBall 13 (rad r) φ
  where
    rad s | s == 1 = 0
          | otherwise = pi

-- As d -> ∞ all angles tend toward pi/2
prop_AnglesTendsToParallel :: Center -> TestCoords -> TestSignPair -> Gen Bool
prop_AnglesTendsToParallel c (Coords (x, z)) (Sigs (s, r)) = do
  let (θ, φ) = snd.ray (10**20) c $ (c + s*x*100, c + r*z*100)
  return $ eBalls 14 (abs θ, abs φ) (pi/2, pi/2)
