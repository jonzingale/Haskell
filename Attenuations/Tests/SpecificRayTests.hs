
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.SpecificRayTests where
import Tests.ExplicitGenerators
import RayTracer.Crossings
import RayTracer.Transport
import Test.Framework

-- Does the scaling make any sense here?
-- moved over 1/2 and then what?
test_sθ_sφ_x_no_z =
  let ijkSeg = transport (1/2, 0) (atan 2, atan(2/sqrt 5)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 13) eval (7 * sqrt (1 + (2/sqrt 5)**2))
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7

test_sθ_sφ_xz =
  let ijkSeg = transport (1/3, 1/4) (atan (3/2), atan(4*sqrt 13 / 9)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 13) eval (1 * 17/12)
  where
    stopCond ((x,y,z), s) = x>=0 && y<1 && z<1

test_sφ_lθ_xz =
  let ijkSeg = transport (3/4, 1/4) (pi-atan(4/5), atan(5/3)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 13) eval (1 * sqrt 34 / 4)
  where
    stopCond ((x,y,z), s) = x>=0 && y<1 && z<1

test_lφ_lθ_xz =
  let ijkSeg = transport (3/4, 4/5) (pi-atan(4/3), atan(25/16)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 13) eval (1 * sqrt 881 / 20)
  where
    stopCond ((x,y,z), s) = x>=0 && y<1 && z>=0

test_lφ_lθ_xz' =
  let ijkSeg = transport (3/4, 2/3) (pi-atan(4/3), atan(15/8)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 13) eval (1 * 17/12)
  where
    stopCond ((x,y,z), s) = x>=0 && y<1 && z>=0

test_lφ_sθ_xz =
  let ijkSeg = transport (1/4, 2/3) (atan (4/3), pi - atan(15/8)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 10) eval (1 * 17/12)
  where
    stopCond ((x,y,z), s) = x>=0 && y<1 && z >= 0

test_arbitrary_coords = -- Not yet good
  let ijkSeg = transport (1/5, 5/6) (pi/4, pi - atan (96*sqrt 2 / 70)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 13) eval (1 * 23332 / 14400)
  where
    stopCond ((x,y,z), s) = x>=0 && y<1 && z >= 0