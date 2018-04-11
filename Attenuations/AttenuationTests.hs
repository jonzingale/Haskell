
-- http://hackage.haskell.org/package/HTF-0.13.2.4/docs/Test-Framework-Tutorial.html
-- http://www.pstcc.edu/departments/natural_behavioral_sciences/Web%20Physics/TRIGG/Chapter(4).htm
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AttenuationTests where
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
