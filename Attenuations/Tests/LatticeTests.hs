
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.LatticeTests where
import Tests.ExplicitGenerators
import RayTracer.FileToVector
import RayTracer.Transport
import RayTracer.Crossings

import qualified Data.Vector.Unboxed as U
import Test.QuickCheck.Monadic
import Test.Framework

-- 3D data files: 7x7x7
allOnes = fileToAry "./Tests/dataAllOnes3D"

-- Arbitrary Lattice Tests
prop_QueryArbitraryLattice :: U.Vector Double -> Gen Bool
prop_QueryArbitraryLattice ary = do
  let rootSize = floor.(** (1/3)).fromIntegral $ U.length ary
  x <- choose (0::Int, rootSize-1)
  y <- choose (0::Int, rootSize-1)
  z <- choose (0::Int, rootSize-1)
  return $ (abs.qArray rootSize (x, y, z)) ary >= 0

-- Ray Tests
prop_exits_y_side :: TestExit -> Gen Bool
prop_exits_y_side (Vars (x, z) (θ, φ)) = do
  let seg = transport (x, z) (θ, φ)
  return $ lastY seg == 7
  where
    stopCond ((x,y,z), s) = x <= 7 && y <= 7 && z <= 7
    lastY trans = last [ j | ((i,j,k), s) <- takeWhile stopCond trans]
