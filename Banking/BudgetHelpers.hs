module BudgetHelpers where
import Text.Regex

type Description = String
type Currency = Float

data Date = Date { year :: Int, month :: Int, day :: Int } deriving (Show, Eq)

date = "07/18/2016"

toDate :: String -> Date
toDate dt = let [y, m, d] = parseDate dt in Date y m d
  where parseDate str = map read $ splitRegex (mkRegex "/") date

toCurrency :: String -> Currency
toCurrency val = read $ subRegex (mkRegex "\\$") val ""

isDebit :: Currency -> Bool
isDebit val = val >= 0