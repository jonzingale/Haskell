
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.TransportTests where
import Tests.ExplicitGenerators
import RayTracer.FileToVector -- fileToAry, qArray, vLength, vSum
import RayTracer.Transport
import Test.Framework

allOnes = fileToAry "./Tests/dataTestAllOnes" -- 7x7
fortyNineDoubles = fileToAry "./Tests/data49Doubles" -- 7x7

test_ArrayIsSevenBySeven = do
  ones <- allOnes
  assertEqual (vLength ones) 49

test_ArrayIsAllOnes = do
  ones <- allOnes
  assertEqual (vSum ones) 49

test_allOnesDiagonal = do
  ary <- allOnes
  let (x, t) = (0, pi/4)
  let (_:ijSeg) = transport x t -- because the head is not necessary.
  let eval = sum [ seg * (qArray 7 ij ary) | (ij, seg) <- takeWhile stopCond ijSeg]
  assertEqual eval (7 * sqrt 2)
  where
    stopCond ((x,y), s) = x<7 && y<7

-- prop_allOnesScales :: Gen Bool
test_allOnesScales = do
  -- θ <- zeroToPi
  -- x <- interval
  let (x, θ) = (0, pi/4)
  ary <- allOnes
  let (_:ijSeg) = transport x θ -- because the head is not necessary.
  let cellEval = (* 7).snd.(!! 1) $ transport (x/7) θ -- (*7).(rl).(/7) == rl
  let latticeEval = sum [ seg * (qArray 7 ij ary) | (ij, seg) <- takeWhile stopCond ijSeg]
  return $ (eBall 13) cellEval latticeEval
  where
    stopCond ((x,y), s) = x<7 && y<7

-- prop_reflectInv :: (Point, Angle) -> Bool
-- prop_reflectInv cs = (mtol.reflectY.reflectY) cs == mtol cs

-- prop_RhoIsReflectedRho' :: Gen Bool
-- prop_RhoIsReflectedRho' = do
--   y <- interval
--   t <- rhoRegion y
--   let (csR, tR) = reflectY ((0,y), t)
--   return $ (eBall 13) (rho (0,y) t) (rho' csR tR)

