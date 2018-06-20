
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.TransportTests3D where
import RayTracer.HelpersTransport3D
import Tests.ExplicitGenerators
import RayTracer.FileToVector -- fileToAry, qArray, vLength, vSum
import RayTracer.Transport3D

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

{--

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

prop_XYZDiagonal :: TestCoords -> Bool
prop_XYZDiagonal = do
  let ijkSeg = transport (0, 0) (pi/4, atan(sqrt 2))
  let eval = sum [ seg | (_, seg) <- takeWhile stopCond ijkSeg]
  return $ (eBall 13) eval (7 * sqrt 3)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7

{--
Translation Tests:
--}
-- Perturbations along x do not effect raylength
-- for rays exiting the rear of the lattice.
prop_allOnesXTranslations :: TestCoords -> Property
prop_allOnesXTranslations (Coords (x, z)) = monadicIO $ do
  ary <- run allOnes
  let ijkSeg1 = transport (x, 0) (atan 2, pi/2)
  let ijkSeg2 = transport (z, 0) (atan 2, pi/2)
  assert $ (eBall 13) (evalRay ijkSeg1 ary) (evalRay ijkSeg2 ary)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7
    evalRay trans ary = sum [ seg * qArray 7 ijk ary |
      (ijk, seg) <- takeWhile stopCond trans]

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

-- better tests for angles and such?
-- prop_allOnesZTranslations :: TestCoords -> Gen Bool
-- prop_allOnesZTranslations (Coords (x, z)) = do
--   θ <- choose((1+x)*pi/4, pi/2)
--   let φ = pi/2.1

--   -- φ <- choose((1+x)*pi/4, pi/2)
--   let ijkSeg1 = transport (0, x) (θ, φ)
--   let ijkSeg2 = transport (0, z) (θ, φ)
--   return $ (eBall 13) (evalRay ijkSeg1) (evalRay ijkSeg2)
--   where
--     stopCond ((x,y,z), s) = x<7 && y<7 && z<7
--     evalRay trans = sum [ seg | (ijk, seg) <- takeWhile stopCond trans]

-- prop_allOnesSmallExitAngleZTranslations :: TestCoords -> Gen Bool
-- prop_allOnesSmallExitAngleZTranslations (Coords (x, z)) = do
--   θ <- choose((1+x)*pi/4, 0)
--   φ <- choose((1+z)*pi/4, 0)
--   let ijkSeg1 = transport (x, x) (θ, φ) -- should be able to perturb here.
--   let ijkSeg2 = transport (x, z) (θ, φ) -- ~pi/2.2 ok
--   return $ (eBall 13) (evalRay ijkSeg1) (evalRay ijkSeg2)
--   where
--     evalRay ts = sum [ seg | ((i,j,k), seg) <- takeWhile stopCond ts]
--     stopCond ((x,y,z), _) = x<5 && y<5 && z<5

upper x = (pi/2) + (pi/4)*x
lower x = (1+x)*pi/4
