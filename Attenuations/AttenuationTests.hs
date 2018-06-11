
-- http://hackage.haskell.org/package/HTF-0.13.2.4/docs/Test-Framework-Tutorial.html
-- http://www.pstcc.edu/departments/natural_behavioral_sciences/Web%20Physics/TRIGG/Chapter(4).htm
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AttenuationTests where
import Tests.ExplicitGenerators
import RayTracer.HelpersTransport3D
import RayTracer.Transport3D
import Test.Framework

import {-@ HTF_TESTS @-} Tests.TransportTests3D

main = htfMain htf_importedTests

transportTests = htfMain htf_Tests_TransportTests3D_thisModulesTests
