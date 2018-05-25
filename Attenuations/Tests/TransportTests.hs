
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.TransportTests where
import Tests.ExplicitGenerators
import RayTracer.FileToVector -- fileToAry, qArray, vLength, vSum
import RayTracer.Transport

import Test.QuickCheck.Monadic
import Test.Framework

allOnes = fileToAry "./Tests/dataTestAllOnes" -- 7x7
fortyNineDoubles = fileToAry "./Tests/data49Doubles" -- 7x7

{--
stopCond ((x,y), s) = x<7 && y<7 && x>0
integrate l a = sum [ seg * qArray 7 ij a |(ij, seg) <- takeWhile stopCond l]
ary <- allOnes
take 10 (transport 3.5 (3*pi/4))
integrate (tail $ transport 3.5 (3*pi/4)) ary
integrate (tail $ transport 3.5 (pi/4)) ary
--} 

-- FullPI Tests
prop_allPosiNegativeWhatever :: TestFullPI -> Property
prop_allPosiNegativeWhatever (FullPI x θ) = monadicIO $ do
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


--HalfPI Tests
prop_allOnesScalesArray :: TestHalfPI -> Property
prop_allOnesScalesArray (HalfPI x θ) = monadicIO $ do
  ary <- run allOnes  -- (*7).(rl).(/7) == rl
  let ijSeg = tail $ transport (x*7) θ  -- because the head is not necessary.
  let cellEval = (* 7).snd.(!! 1) $ transport x θ
  let latticeEval = sum [ seg * qArray 7 ij ary |
        (ij, seg) <- takeWhile stopCond ijSeg]

  assert $ (eBall 13) cellEval latticeEval
  where
    stopCond ((x,y), s) = x<7 && y<7

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
