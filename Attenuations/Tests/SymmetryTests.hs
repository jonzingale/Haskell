{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.SymmetryTests where
import Tests.ExplicitGenerators
import RayTracer.FileToVector -- fileToAry, qArray, vLength, vSum
import RayTracer.Transport
import RayTracer.Crossings

import qualified Data.Vector.Unboxed as U
import Test.QuickCheck.Monadic
import Test.Framework

-- 3D data files: 7x7x7
allOnes = fileToAry "./Tests/dataAllOnes3D"
gradientDoubles = fileToAry "./Tests/dataGradArray3D"
stratifiedDoubles = fileToAry "./Tests/dataStratifiedArray3D"

{--
  Axis Rotation Tests:
--}

mirrorCoords :: (XCoord, Angle) -> (XCoord, Angle)
mirrorCoords (x, θ) = (7-x, pi-θ)

prop_gradientArrayXSymmetry :: TestRay -> Property
prop_gradientArrayXSymmetry (Ray (x, z) (θ, φ)) = monadicIO $ do
  ary <- run gradientDoubles
  let ijkSeg = transport (x, z) (θ, pi/2)
  let (x', θ') = mirrorCoords (x, θ)
  let pqSeg = transport (mirrorCoords (x', z)) (θ', pi/2)

  assert $ (eBall 13) (integrate ijkSeg ary) (integrate pqSeg ary)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7 && x>0 && y>0 && z>0
    integrate l a = sum [ seg * qArray 7 ijk a |
        (ijk, seg) <- takeWhile stopCond l]

prop_gradientArrayZSymmetry :: TestRay -> Property
prop_gradientArrayZSymmetry (Ray (x, z) (θ, φ)) = monadicIO $ do
  ary <- run gradientDoubles
  let ijkSeg = transport (x, z) (pi/4, φ)
  let (z', φ') = mirrorCoords (z, φ)
  let pqSeg = transport (mirrorCoords (x, z')) (pi/4, φ')

  assert $ (eBall 13) (integrate ijkSeg ary) (integrate pqSeg ary)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7 && x>0 && y>0 && z>0
    integrate l a = sum [ seg * qArray 7 ijk a |
        (ijk, seg) <- takeWhile stopCond l]

prop_stratifiedArrayXSymmetry :: TestRay -> Property
prop_stratifiedArrayXSymmetry (Ray (x, z) (θ, φ)) = monadicIO $ do
  ary <- run stratifiedDoubles
  let ijkSeg = transport (x, z) (θ, pi/2)
  let (x', θ') = mirrorCoords (x, θ)
  let pqSeg = transport (mirrorCoords (x', z)) (θ', pi/2)

  assert $ (eBall 13) (integrate ijkSeg ary) (integrate pqSeg ary)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && x>0
    integrate l a = sum [ seg * qArray 7 ij a |
        (ij, seg) <- takeWhile stopCond l]

prop_stratifiedArrayZSymmetry :: TestRay -> Property
prop_stratifiedArrayZSymmetry (Ray (x, z) (θ, φ)) = monadicIO $ do
  ary <- run stratifiedDoubles
  let ijkSeg = transport (x, z) (θ, pi/2)
  let (z', φ') = mirrorCoords (z, φ)
  let pqSeg = transport (mirrorCoords (x, z')) (pi/4, φ')

  assert $ (eBall 13) (integrate ijkSeg ary) (integrate pqSeg ary)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z>0
    integrate l a = sum [ seg * qArray 7 ij a |
        (ij, seg) <- takeWhile stopCond l]

prop_allOnesXSymmetry :: TestRay -> Property
prop_allOnesXSymmetry (Ray (x, z) (θ, φ)) = monadicIO $ do
  ary <- run allOnes
  let ijkSeg = transport (x, z) (θ, pi/2)
  let (x', θ') = mirrorCoords (x, θ)
  let pqSeg = transport (mirrorCoords (x', z)) (θ', pi/2)

  assert $ (eBall 13) (integrate ijkSeg ary) (integrate pqSeg ary)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && x>0
    integrate l a = sum [ seg * qArray 7 ij a |
        (ij, seg) <- takeWhile stopCond l]

prop_allOnesZSymmetry :: TestRay -> Property
prop_allOnesZSymmetry (Ray (x, z) (θ, φ)) = monadicIO $ do
  ary <- run allOnes
  let ijkSeg = transport (x, z) (pi/4, φ)
  let (z', φ') = mirrorCoords (z, φ)
  let pqSeg = transport (mirrorCoords (x, z')) (pi/4, φ')

  assert $ (eBall 13) (integrate ijkSeg ary) (integrate pqSeg ary)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z>0
    integrate l a = sum [ seg * qArray 7 ij a |
        (ij, seg) <- takeWhile stopCond l]

{--
Translation Tests:
--}

prop_Smallθ_XTranslations :: TestCoords -> Gen Bool
prop_Smallθ_XTranslations (Coords (x, z)) = do
  x'<- choose (x, 1)
  θ <- choose ((1+x')*pi/4, pi/2)
  φ <- choose (0, pi/2)
  let seg1 = transport (3.5*x , 0) (θ, φ)
  let seg2 = transport (3.5*x', 0) (θ, φ)
  return $ (eBall 13) (evalRay seg1) (evalRay seg2)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7
    evalRay trans = sum [ seg | (ijk, seg) <- takeWhile stopCond trans]

prop_Smallφ_ZTranslations :: TestCoords -> Gen Bool
prop_Smallφ_ZTranslations (Coords (x, z)) = do
  z'<- choose (z, 1)
  θ <- choose (0, pi/2)
  φ <- choose ((1+z')*pi/4, pi/2)
  let seg1 = transport (0, 3.5*z ) (θ, φ)
  let seg2 = transport (0, 3.5*z') (θ, φ)
  return $ (eBall 13) (evalRay seg1) (evalRay seg2)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7
    evalRay trans = sum [ seg | (ijk, seg) <- takeWhile stopCond trans]

prop_Largeθ_XTranslations :: TestCoords -> Gen Bool
prop_Largeθ_XTranslations (Coords (x, z)) = do
  x'<- choose (x, 1)
  θ <- choose (pi/2, pi/2 + x*pi/4)
  φ <- choose (θ, pi)
  let seg1 = transport (3.5*x , 0) (θ, φ)
  let seg2 = transport (3.5*x', 0) (θ, φ)
  return $ (eBall 12) (evalRay seg1) (evalRay seg2)
  where
    stopCond ((x,y,z), s) = x >= 0 && y < 7 && z < 7 && z >= 0
    evalRay trans = sum [ seg | (ijk, seg) <- takeWhile stopCond trans]

prop_Largeφ_ZTranslations :: TestCoords -> Gen Bool
prop_Largeφ_ZTranslations (Coords (x, z)) = do
  z'<- choose (z, 1)
  φ <- choose (pi/2, pi/2 + z*pi/4)
  θ <- choose (φ, pi/2) -- what range here?
  let seg1 = transport (7*z , 0) (θ, φ)
  let seg2 = transport (7*z', 0) (θ, φ)
  return $ (eBall 13) (evalRay seg1) (evalRay seg2)
  where
    stopCond ((x,y,z), s) = x < 7 && y < 7 && z >= 0
    evalRay trans = sum [ seg | (ijk, seg) <- takeWhile stopCond trans]

-- better tests for angles and such?
prop_allOnesHalfPiXTranslations :: TestCoords -> Property
prop_allOnesHalfPiXTranslations (Coords (x, z)) = monadicIO $ do
  ary <- run allOnes
  let ijkSeg1 = transport (x*7/2, 0) (atan 2, pi/2)
  let ijkSeg2 = transport (z*7/2, 0) (atan 2, pi/2)
  assert $ (eBall 13) (evalRay ijkSeg1 ary) (evalRay ijkSeg2 ary)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7
    evalRay trans ary = sum [ seg * qArray 7 ijk ary |
      (ijk, seg) <- takeWhile stopCond trans]