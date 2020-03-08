module Budget where
import qualified Data.ByteString.Lazy as BL

import BudgetHelpers
import BankParser

main = do
  oneYear <- BL.readFile "./one_year.csv"
  let statements = bankRecords oneYear
  let balances = map (toCurrency.balance) statements
  print balances