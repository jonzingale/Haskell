-- http://hackage.haskell.org/package/HTF-0.13.2.4/docs/Test-Framework-Tutorial.html
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module BudgetTests where
import BudgetHelpers
import Test.Framework

import {-@ HTF_TESTS @-} Test.BudgetHelpersTest

main = htfMain htf_importedTests

budgetHelpersTests = htfMain htf_Test_BudgetHelpersTest_thisModulesTests

{--
Running tests:

Starting from BudgetTests.hs root directory
$ ghci BudgetTests.hs
𝜆> main

--}