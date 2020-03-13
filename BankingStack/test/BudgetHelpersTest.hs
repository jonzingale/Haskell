{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.BudgetHelpersTest where
import qualified Data.ByteString.Lazy as BL
import BudgetHelpers
import BankParser

import Test.QuickCheck.Monadic
import Test.Framework
import qualified Test.HUnit as HU -- no examples yet
import Test.Generators
import Text.Printf

oneYear = do
  summary <- BL.readFile "data/one_year.csv"
  return summary

{--
Testing locally to this file:
HUnit: runTest test_toCurrency
QCheck: quickCheck prop_debit_credit_sum_is_correct
--}

test_toCurrency = assertBool $ toCurrency "-$12.00" == -12.0
test_toDate = assertBool $ toDate "2/32/2020" == Date 2 32 2020 

prop_isDebit :: TestCurrency -> Property
prop_isDebit (Currency d c) = monadicIO $ do
  let cBool = if d > 0 then (not.isDebit) d else isDebit d
  assert cBool

prop_toCurrency :: TestCurrency -> Property
prop_toCurrency (Currency d c) = monadicIO $ do
  assert $ (toCurrency.to_str) (d + c) == (d + c)
  where
    to_str dc | dc > 0 = printf "$%f" dc
              | otherwise = printf "-$%f" $ abs dc

prop_debit_credit_sum_is_correct :: Property
prop_debit_credit_sum_is_correct = monadicIO $ do
  ary <- run oneYear
  let statements = bankRecords ary
  let total = map (toCurrency.debit_credit) statements
  assert $ sum total == 8100.965

prop_column_lengths_are_correct :: HeaderSelector -> Property
prop_column_lengths_are_correct (Header x) = monadicIO $ do
  summary <- run oneYear
  let header = [date, description, debit_credit, balance] !! x
  let statements = bankRecords summary
  let column = map header statements
  assert $ length column == 4018
