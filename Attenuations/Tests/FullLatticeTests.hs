{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.FullLatticeTests where
import Tests.ExplicitGenerators
import RayTracer.FileToVector -- fileToAry, qArray, vLength, vSum
import RayTracer.Transport
import RayTracer.Crossings

import qualified Data.Vector.Unboxed as U
import Test.QuickCheck.Monadic
import Test.Framework

-- 3D data files: 7x7x7
allOnes = fileToAry "./Tests/dataAllOnes3D"

prop_fullLattice :: Property
prop_fullLattice = monadicIO $ do
  ary <- run allOnes
  let ijkSeg = transport (0, 0) (pi/4, atan(sqrt 2))
  let eval = sum [ seg * qArray 7 ijk ary | (ijk, seg) <- takeWhile stopCond ijkSeg]
  assert $ (eBall 13) eval (7 * sqrt 3)
  where
    stopCond ((x,y,z), s) = x<7 && y<7 && z<7
