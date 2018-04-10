
-- http://hackage.haskell.org/package/HTF-0.13.2.4/docs/Test-Framework-Tutorial.html
-- http://www.pstcc.edu/departments/natural_behavioral_sciences/Web%20Physics/TRIGG/Chapter(4).htm
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AttenuationTest where
import RayTracer.RayLength
import RayTracer.Lattice
import RayTracer.Rhythm
import Test.Framework

import {-@ HTF_TESTS @-} Tests.IndexerTests
import {-@ HTF_TESTS @-} Tests.SymmetryTests
import {-@ HTF_TESTS @-} Tests.XRegionTests

main = htfMain htf_importedTests

symmetryTests = htfMain htf_Tests_SymmetryTests_thisModulesTests
indexerTests = htfMain htf_Tests_IndexerTests_thisModulesTests
xRegionTests = htfMain htf_Tests_XRegionTests_thisModulesTests


replayArg = "Just (TFGenR 62B8735EAF47B2F9B36E26801D8A7573F7F28A1DF3A296D236CCC934F7064674 0 1 1 0,0)"

prop_Replay =
  withQCArgs (\a -> a { replay = read replayArg })
  prop_RotRhoIsEps

replayTest = htfMain htf_AttenuationTest_thisModulesTests
