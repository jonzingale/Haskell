{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.BudgetHelpersTest where
import qualified Data.ByteString.Lazy as BL
import BudgetHelpers
import BankParser

import Test.QuickCheck.Monadic
import Test.Framework

oneYear = do
  summary <- BL.readFile "./one_year.csv"
  return summary

data HeaderSelector = Header Int deriving (Show, Eq)

instance Arbitrary HeaderSelector where
  arbitrary = do
    x <- choose (0, 3::Int)
    return $ Header x

-- USAGE: runTest test_toCurrency
test_toCurrency = assertBool $ toCurrency "-$12.00" == -12.0

-- USAGE: quickCheck prop_debit_credit_sum_is_correct
prop_debit_credit_sum_is_correct :: Property
prop_debit_credit_sum_is_correct = monadicIO $ do
  ary <- run oneYear
  let statements = bankRecords ary
  let total = map (toCurrency.debit_credit) statements
  assert $ sum total == 8100.965

-- USAGE: quickCheck prop_column_lengths_are_correct
prop_column_lengths_are_correct :: HeaderSelector -> Property
prop_column_lengths_are_correct (Header x) = monadicIO $ do
  summary <- run oneYear
  let header = [date, description, debit_credit, balance] !! x
  let statements = bankRecords summary
  let column = map header statements
  assert $ length column == 4018
