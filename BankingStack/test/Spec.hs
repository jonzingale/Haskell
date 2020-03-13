import BudgetHelpers
import Test.Framework

import {-@ HTF_TESTS @-} BudgetHelpersTest

main = htfMain htf_importedTests

budgetHelpersTests = htfMain htf_BudgetHelpersTest_thisModulesTests