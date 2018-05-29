
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.TransportTests where
import Tests.ExplicitGenerators
import RayTracer.FileToVector -- fileToAry, qArray, vLength, vSum
import RayTracer.Transport3D

import qualified Data.Vector.Unboxed as U
import Test.QuickCheck.Monadic
import Test.Framework

allOnes = fileToAry "./Tests/dataTestAllOnes" -- 7x7
gradientDoubles = fileToAry "./Tests/dataGradArray" -- 7x7
stratifiedDoubles = fileToAry "./Tests/dataStratifiedArray" -- 7x7

-- Arbitrary Lattice Tests
prop_QueryArbitraryLattice :: U.Vector Double -> Gen Bool
prop_QueryArbitraryLattice ary = do
  let rootSize = floor.sqrt.fromIntegral $ U.length ary
  x <- choose (0::Int, rootSize-1)
  y <- choose (0::Int, rootSize-1)
  return $ (abs.qArray rootSize (x, y)) ary >= 0

-- Ray Tests
prop_gradientArraySymmetry :: TestRay -> Property
prop_gradientArraySymmetry (Ray x θ) = monadicIO $ do
  ary <- run gradientDoubles
  let ijSeg = tail $ transport x θ -- because the head is not necessary.
  let pqSeg = tail $ uncurry transport $ mirrorCoords (x, θ)

  assert $ (eBall 13) (integrate ijSeg ary) (integrate pqSeg ary)
  where
    stopCond ((x,y), s) = x<7 && y<7 && x>0
    integrate l a = sum [ seg * qArray 7 ij a |
        (ij, seg) <- takeWhile stopCond l]

prop_stratifiedArraySymmetry :: TestRay -> Property
prop_stratifiedArraySymmetry (Ray x θ) = monadicIO $ do
  ary <- run stratifiedDoubles
  let ijSeg = tail $ transport x θ -- because the head is not necessary.
  let pqSeg = tail $ uncurry transport $ mirrorCoords (x, θ)

  assert $ (eBall 13) (integrate ijSeg ary) (integrate pqSeg ary)
  where
    stopCond ((x,y), s) = x<7 && y<7 && x>0
    integrate l a = sum [ seg * qArray 7 ij a |
        (ij, seg) <- takeWhile stopCond l]

prop_allOnesSymmetry :: TestRay -> Property
prop_allOnesSymmetry (Ray x θ) = monadicIO $ do
  ary <- run allOnes
  let ijSeg = tail $ transport x θ -- because the head is not necessary.
  let pqSeg = tail $ uncurry transport $ mirrorCoords (x, θ)

  assert $ (eBall 13) (integrate ijSeg ary) (integrate pqSeg ary)
  where
    stopCond ((x,y), s) = x<7 && y<7 && x>0
    integrate l a = sum [ seg * qArray 7 ij a |
        (ij, seg) <- takeWhile stopCond l]


mirrorCoords :: (XCoord, Angle) -> (XCoord, Angle)
mirrorCoords (x, θ) = (7-x, pi-θ)

prop_mirrorCoordsSelfInverse :: Gen Bool
prop_mirrorCoordsSelfInverse = do
  x <- choose (0, 7::Double)
  t <- zeroToPi
  let (y, s) = mirrorCoords.mirrorCoords $ (x, t)
  return $ (eBall 13) x y && (eBall 13) s t

test_ArrayIsSevenBySeven = do
  ones <- allOnes
  assertEqual (vLength ones) 49

test_ArrayIsAllOnes = do
  ones <- allOnes
  assertEqual (vSum ones) 49

test_allOnesDiagonal = do
  ary <- allOnes
  let (x, t) = (0, pi/4)
  let ijSeg = tail $ transport x t -- because the head is not necessary.
  let eval = sum [ seg * qArray 7 ij ary | (ij, seg) <- takeWhile stopCond ijSeg]
  assertEqual eval (7 * sqrt 2)
  where
    stopCond ((x,y), s) = x<7 && y<7

