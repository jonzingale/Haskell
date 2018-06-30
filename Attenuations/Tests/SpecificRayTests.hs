
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.SpecificRayTests where
import Tests.ExplicitGenerators
import RayTracer.Crossings
import RayTracer.Transport
import Test.Framework

-- Does the scaling make any sense here?
-- moved over 1/2 and then what?
test_smallθφ_x_no_z =
  let ijkSeg = transport (1/2, 0) (atan 2, atan(2/sqrt 5)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 13) eval (7 * sqrt (1 + (2/sqrt 5)**2))
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7

-- This doesn't seem to work. enters through the middle and ought
-- to exit through the uppercorner.
test_smallθφ_xz =
  let ijkSeg = transport (1/2, 1/2) (pi - atan 2, atan(1/sqrt 5)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 10) eval (7 * sqrt ((0.5 * sqrt 5)**2 + (0.5)**4))
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7

