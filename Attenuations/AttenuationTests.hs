
-- http://hackage.haskell.org/package/HTF-0.13.2.4/docs/Test-Framework-Tutorial.html
-- http://www.pstcc.edu/departments/natural_behavioral_sciences/Web%20Physics/TRIGG/Chapter(4).htm
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AttenuationTests where
import Test.Framework

import {-@ HTF_TESTS @-} Tests.PureComponentsTests
import {-@ HTF_TESTS @-} Tests.LatticeTests
import {-@ HTF_TESTS @-} Tests.SymmetryTests
import {-@ HTF_TESTS @-} Tests.DiagonalTests

main = htfMain htf_importedTests

pureComponentsTests = htfMain htf_Tests_PureComponentsTests_thisModulesTests
latticeTests = htfMain htf_Tests_LatticeTests_thisModulesTests
symmetryTests = htfMain htf_Tests_SymmetryTests_thisModulesTests
diagonalTests = htfMain htf_Tests_DiagonalTests_thisModulesTests
