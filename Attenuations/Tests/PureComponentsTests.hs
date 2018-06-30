{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.PureComponentsTests where
import Tests.ExplicitGenerators
import RayTracer.FileToVector -- fileToAry, qArray, vLength, vSum
import RayTracer.Transport
import RayTracer.Crossings

import qualified Data.Vector.Unboxed as U
import Test.QuickCheck.Monadic
import Test.Framework

-- 3D data files: 7x7x7
allOnes = fileToAry "./Tests/dataAllOnes3D"

{-- 
  Pure Component Tests:

  φ determines projection:
  φ == 0 or φ == pi => pure z component
  φ == π/2 => pure x-y components
--}

prop_pureZComponent (Coords (x, z)) (Angle θ) = do
  φ <- oneof [return 0, return pi]
  s <- choose (3, 100::Double)

  let ijkSeg = take 30 $ transport (x*s, z*s) (θ, φ)
  return $ all (pureZcond x s) ijkSeg
  where    
    pureZcond x s ((i,j,k), _) =
      and [ i == floor (x * s), j == 0]

prop_pureXYComponents (Coords (x, z)) (Angle θ) = do
  s <- choose (3, 100::Double)
  let ijkSeg = take 30 $ transport (x*s, z*s) (θ, pi/2)
  return $ all (pureZcond (z*s)) ijkSeg
  where    
    pureZcond zz ((i,j,k), _) =
      k == floor zz

prop_pureXZComponents (Coords (x, z)) (Angle θ) = do
  s <- choose (3, 100::Double)
  let ijkSeg = take 30 $ transport (x*s, z*s) (0, θ)
  return $ all pureZcond ijkSeg
  where    
    pureZcond ((i,j,k), _) =
      j == 0

prop_pureYZComponents (Coords (x, z)) (Angle θ) = do
  s <- choose (3, 100::Double)
  let ijkSeg = take 30 $ transport (x*s, z*s) (pi/2, θ)
  return $ all (pureZcond (x*s)) ijkSeg
  where    
    pureZcond xx ((i,j,k), _) =
      i == floor xx

{--
  fixing φ == π/2
  θ == 0 or θ == π => pure x component
  θ == π/2 ==> pure y component
--}

prop_PureXComponent (Coords (x, z)) = do
  s <- choose (3, 100::Double)
  θ <- oneof [return 0, return pi]
  let ijkSeg = take 10 $ transport (x*s, z*s) (θ, pi/2)
  return $ all (pureXCond (x*s, z*s)) ijkSeg
  where    
    pureXCond (xx, zz) ((i,j,k), _) =
      j == 0 && k == floor zz

prop_ascending_PureXComponent = do
  x <- interval
  z <- interval
  let ijkSeg = take 10 $ transport (x, z) (0, pi/2)
  return $ map xComponent ijkSeg == [0..9]
  where    
    xComponent ((i,j,k), _) = i

prop_descending_PureXComponent = do
  x <- interval
  z <- interval
  let ijkSeg = take 10 $ transport (x, z) (pi, pi/2)
  return $ map xComponent ijkSeg == map negate [0..9]
  where    
    xComponent ((i,j,k), _) = i

-- y component
prop_pureYComponent (Coords (x, z)) = do
  s <- choose (3, 100::Double)
  let ijkSeg = take 30 $ transport (x*s, z*s) (pi/2, pi/2)
  return $ all (pureYCond (x*s, z*s)) ijkSeg
  where    
    pureYCond (xx, zz) ((i,j,k), _) =
      i == floor xx && k == floor zz

{--
  fixing φ == 0 or φ == pi
  x - y plane is left invariant.
--}

prop_PureZComponent (Coords (x, z)) = do
  s <- choose (3, 100::Double)
  φ <- oneof [return 0, return pi]
  θ <- zeroToPi
  let ijkSeg = take 10 $ transport (x*s, z*s) (θ, φ)
  return $ all (pureZCond (x*s)) ijkSeg
  where    
    pureZCond xx ((i,j,k), _) =
      i == floor xx && j == 0

prop_ascending_PureZComponent (Coords (x, z)) = do
  θ <- zeroToPi
  let ijkSeg = take 10 $ transport (x, z) (θ, 0)
  return $ map zComponent ijkSeg == [0..9]
  where    
    zComponent ((i,j,k), _) = k

prop_descending_PureZComponent (Coords (x, z)) = do
  θ <- zeroToPi
  let ijkSeg = take 10 $ transport (x, z) (θ, pi)
  return $ map zComponent ijkSeg == map negate [0..9]
  where    
    zComponent ((i,j,k), _) = k


-- Segment Verification
prop_allOnesXs :: TestCoords -> Property
prop_allOnesXs (Coords (x, z)) = monadicIO $ do
  ary <- run allOnes
  let ijkSeg = transport (x, z) (0, pi/2)
  let eval = sum [ seg * qArray 7 ijk ary | (ijk, seg) <- takeWhile stopCond ijkSeg]
  assert $ (eBall 13) eval (7 - x)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7

prop_allOnesYs :: TestCoords -> Property
prop_allOnesYs (Coords (x, z)) = monadicIO $ do
  ary <- run allOnes
  let ijkSeg = transport (x, z) (pi/2, pi/2)
  let eval = sum [ seg * qArray 7 ijk ary | (ijk, seg) <- takeWhile stopCond ijkSeg]
  assert $ eval == 7
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7

prop_allOnesZs :: TestAngle -> Property
prop_allOnesZs (Angle θ) = monadicIO $ do
  ary <- run allOnes
  let ijkSeg = transport (0, 0) (θ, 0)
  let eval = sum [ seg * qArray 7 ijk ary | (ijk, seg) <- takeWhile stopCond ijkSeg]
  assert $ eval == 7
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7