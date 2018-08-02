{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.DiagonalTests where
import Tests.ExplicitGenerators
import RayTracer.FileToVector
import RayTracer.Transport
import RayTracer.Crossings

import qualified Data.Vector.Unboxed as U
import Test.QuickCheck.Monadic
import Test.Framework

-- 3D data files: 7x7x7
allOnes = fileToAry "./Tests/dataAllOnes3D"

prop_allOnesXYDiagonal :: TestCoords -> Property
prop_allOnesXYDiagonal (Coords (x, z)) = monadicIO $ do
  ary <- run allOnes
  let ijkSeg = transport (0, z) (pi/4, pi/2)
  let eval = sum [ seg * qArray 7 ijk ary | (ijk, seg) <- takeWhile stopCond ijkSeg]
  assert $ (eBall 13) eval (7 * sqrt 2)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7

prop_allOnesXYZDiagonal :: Property
prop_allOnesXYZDiagonal = monadicIO $ do
  ary <- run allOnes
  let ijkSeg = transport (0, 0) (pi/4, atan(sqrt 2))
  let eval = sum [ seg * qArray 7 ijk ary | (ijk, seg) <- takeWhile stopCond ijkSeg]
  assert $ (eBall 13) eval (7 * sqrt 3)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7

test_XYZDiagonal =
  let ijkSeg = transport (0, 0) (pi/4, atan(sqrt 2)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 13) eval (7 * sqrt 3)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7

test_XYZDiagonal2 =
  let ijkSeg = transport (7, 0) (3*pi/4, atan(sqrt 2)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 13) eval (7 * sqrt 3)
  where
    stopCond ((x,y,z), s) = y<7 && z<7

test_XYZDiagonal3 =
  let ijkSeg = transport (0, 7) (pi/4, pi - atan(sqrt 2)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 13) eval (7 * sqrt 3)
  where
    stopCond ((x,y,z), s) = y<7 && x<7 && z > 0

test_XYZDiagonal4 =
  let ijkSeg = transport (7, 7) (3*pi/4, pi - atan(sqrt 2)) in
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg] in
  assertBool $ (eBall 13) eval (7 * sqrt 3)
  where
    stopCond ((x,y,z), s) = z > 0 --- y<7 && x<7 &&