
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.TransportTests where
import Tests.ExplicitGenerators
import RayTracer.FileToVector -- fileToAry, qArray, vLength, vSum
import RayTracer.Transport
import Test.Framework

allOnes = fileToAry "./Tests/dataTestAllOnes"
fortyNineDoubles = fileToAry "./Tests/data49Doubles"

test_ArrayIsSevenBySeven = do
  ones <- allOnes
  assertEqual (vLength ones) 49

test_ArrayIsAllOnes = do
  ones <- allOnes
  assertEqual (vSum ones) 49

test_OnesTransport = do
  ary <- allOnes
  let (x, t) = (0, pi/4)
  let (_:ijSeg) = transport x t -- because the head is not necessary.
  let eval = sum [ seg * (qArray 7 ij ary) | (ij, seg) <- takeWhile stopCond ijSeg]
  assertEqual eval (7 * sqrt 2)
  where
    stopCond ((x,y), s) = x<7 && y<7

-- prop_rot4Id :: Point -> Gen Bool
-- prop_rot4Id cs = do
--   t <- zeroToTau
--   return $ (mtol.diffPi.rotFour) (cs, t) == mtol (cs, t)
--   where
--     diffPi ((x, y), theta) = ((x, y), theta - tau)
--     rotFour = foldr (.) id [rot90 | x<-[1..4]]


-- prop_reflectInv :: (Point, Angle) -> Bool
-- prop_reflectInv cs = (mtol.reflectY.reflectY) cs == mtol cs

-- prop_RhoIsReflectedRho' :: Gen Bool
-- prop_RhoIsReflectedRho' = do
--   y <- interval
--   t <- rhoRegion y
--   let (csR, tR) = reflectY ((0,y), t)
--   return $ (eBall 13) (rho (0,y) t) (rho' csR tR)

