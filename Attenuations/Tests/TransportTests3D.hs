
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.TransportTests3D where
import RayTracer.HelpersTransport3D
import Tests.ExplicitGenerators
import RayTracer.FileToVector -- fileToAry, qArray, vLength, vSum
import RayTracer.Transport3D
import RayTracer.Crossings

import qualified Data.Vector.Unboxed as U
import Test.QuickCheck.Monadic
import Test.Framework

-- 3D data files: 7x7x7
allOnes = fileToAry "./Tests/dataallOnes3D"
gradientDoubles = fileToAry "./Tests/datagradArray3D"

-- General Array Tests.
test_ArrayIsSevenCubed = do
  ones <- allOnes
  assertEqual (vLength ones) 343

test_ArrayIsAllOnes = do
  ones <- allOnes
  assertEqual (vSum ones) 343

-- Arbitrary Lattice Tests
prop_QueryArbitraryLattice :: U.Vector Double -> Gen Bool
prop_QueryArbitraryLattice ary = do
  let rootSize = floor.(** (1/3)).fromIntegral $ U.length ary
  x <- choose (0::Int, rootSize-1)
  y <- choose (0::Int, rootSize-1)
  z <- choose (0::Int, rootSize-1)
  return $ (abs.qArray rootSize (x, y, z)) ary >= 0


-- Ray Tests
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

{--

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

prop_mirrorCoordsSelfInverse :: Gen Bool
prop_mirrorCoordsSelfInverse = do
  x <- choose (0, 7::Double)
  t <- zeroToPi
  let (y, s) = mirrorCoords.mirrorCoords $ (x, t)
  return $ (eBall 13) x y && (eBall 13) s t

--}

{-- 
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

{--
Calculates above segments correctly, does NOT calculate
the array lookups correctly.
--}

-- testYs (0.2754253935112436,0.8388873136908843) (1.0284384107089382,1.651464355058737)
testYs cs as = [(i,j,k) | ((i,j,k), s) <- takeWhile stopCond $ transport cs as]
  where
    stopCond ((x,y,z), s) = abs x < 7 && abs y < 7 && abs z < 7


{--
Translation Tests:
--}
-- Perturbations along x do not effect raylength
-- for rays exiting the rear of the lattice.

-- cheapSums (0,0.0) (atan 2, atan (sqrt 5))
prop_Smallθ_XTranslations :: TestCoords -> Gen Bool
prop_Smallθ_XTranslations (Coords (x, z)) = do
  x'<- choose (x, 1)
  θ <- choose ((1+x')*pi/4, pi/2)
  φ <- choose (0, θ) -- what range here?
  let seg1 = transport (3.5*x , 0) (θ, φ)
  let seg2 = transport (3.5*x', 0) (θ, φ)
  return $ (eBall 13) (evalRay seg1) (evalRay seg2)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7
    evalRay trans = sum [ seg | (ijk, seg) <- takeWhile stopCond trans]

prop_Largeθ_XTranslations :: TestCoords -> Gen Bool
prop_Largeθ_XTranslations (Coords (x, z)) = do
  x'<- choose (x, 1)
  θ <- choose (pi/2, pi/2 + x*pi/4)
  φ <- choose (θ, pi/2) -- what range here?
  let seg1 = transport (3.5*x , 0) (θ, φ)
  let seg2 = transport (3.5*x', 0) (θ, φ)
  return $ (eBall 12) (evalRay seg1) (evalRay seg2)
  where
    stopCond ((x,y,z), s) = abs x < 7 && abs y < 7 && abs z < 7
    evalRay trans = sum [ seg | (ijk, seg) <- takeWhile stopCond trans]

-- better tests for angles and such?
prop_Smallφ_ZTranslations :: TestCoords -> Gen Bool
prop_Smallφ_ZTranslations (Coords (x, z)) = do
  z'<- choose (z, 1)
  φ <- choose ((1+z')*pi/4, pi/2)
  θ <- choose (φ, pi/2) -- what range here?
  let seg1 = transport (0, 7*z ) (θ, φ)
  let seg2 = transport (0, 7*z') (θ, φ)
  return $ (eBall 13) (evalRay seg1) (evalRay seg2)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7
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
    stopCond ((x,y,z), s) = abs x < 7 && abs y < 7 && abs z < 7
    evalRay trans = sum [ seg | (ijk, seg) <- takeWhile stopCond trans]

prop_exits_y_side :: TestExit -> Gen Bool
prop_exits_y_side (Vars (x, z) (θ, φ)) = do
  let seg = transport (x, z) (θ, φ)

  return $ lastY seg == 6
  where
    stopCond ((x,y,z), s) = abs x < 7 && abs y < 7 && abs z < 7
    lastY trans = last [ j | ((i,j,k), s) <- takeWhile stopCond trans]

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
