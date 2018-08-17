{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.GaussianTests where
import RayTracer.CumulativeDistribution (neededRays)
import Data.Random.Normal (mkNormals')

import RayTracer.Constants (center, size)
import Tests.ExplicitGenerators
import RayTracer.GaussianBeam
import RayTracer.Transport
import Test.Framework

{--
Scaling and Translation Tests:
--}

rLen :: Double -> Double -> Double
rLen x y = sqrt $ x**2 + y**2

-- impure as ray depends on center located in Constants.
prop_PullbackPushforwardID :: Gen Bool
prop_PullbackPushforwardID = do
  let size' = (fromIntegral size)::Double
  let (cs, as) = ray 2 (1,0)
  let seg = transport cs as
  let stopCond s ((x,y,z), _) = x< s &&  z< s
  let integrate l = sum [ seg | (_, seg) <- takeWhile (stopCond size) l]
  return $ (eBall 10) (integrate seg) (rLen (center/2) size')

{--
Cumulative Distribution Tests:
--}

prop_NearlyOneThousandRays :: StdDev -> Gen Bool
prop_NearlyOneThousandRays (Dev σ) = do
  let needed = neededRays 1000 σ
  let pts = take needed $ mkNormals' (0, σ) 32
  return $ 940 < (length.filter cond) pts
  where cond x = abs x < 1

{--
Angle Tests:
--}
-- Given a unit cone, diagonals are what I expect.
prop_NormalDiagonals :: TestDistance -> TestSignPair -> Gen Bool
prop_NormalDiagonals (Distance d) (Sigs (s, r)) = do
  let (θ, φ) = snd.ray (d-2) $ (s * d * dd, r * d * dd)
  return $ eBalls 14 (θ, φ) (rad s, rad r)
  where
    dd = sqrt 2 / 2
    rad s | s == 1 = atan (1 / dd)
          | otherwise = pi - atan (1 / dd)

-- Given a unit cone, X and Z components in equal parts give 45s.
prop_EqualComponents :: TestDistance -> TestSignPair -> Gen Bool
prop_EqualComponents (Distance d) (Sigs (s, r)) = do
  let (θ, φ) = snd.ray (d-2) $ (s*d, r*d)
  return $ eBalls 14 (θ, φ) (rad s, rad r)
  where
    rad s | s == 1 = pi/4
          | otherwise = 3*pi/4

-- As d -> exit plane, angles tend toward 0, π, or π/2.
prop_AngleSpraysAway :: TestCoords -> TestSignPair -> TestDistance -> Gen Bool
prop_AngleSpraysAway (Coords (x, z)) (Sigs (s, r)) (Distance c) = do
  let (θ, φ) = snd.ray (-2) $ (s*x, r*z)
  return $ c == 0 || eBall 14 (rad s) θ && eBall 13 (rad r) φ
  where
    rad s | s == 1 = 0
          | otherwise = pi

-- As d -> ∞ all angles tend toward pi/2
prop_AnglesTendsToParallel :: TestCoords -> TestSignPair -> Gen Bool
prop_AnglesTendsToParallel (Coords (x, z)) (Sigs (s, r)) = do
  let (θ, φ) = snd.ray (10**20) $ (s*x, r*z)
  return $ eBalls 14 (abs θ, abs φ) (pi/2, pi/2)
