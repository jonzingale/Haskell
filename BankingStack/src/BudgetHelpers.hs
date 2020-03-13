module BudgetHelpers where
import Text.Regex

type Description = String
type Currency = Float

data Date = Date { year :: Int, month :: Int, day :: Int } deriving (Show, Eq)

toDate :: String -> Date
toDate dt = let [y, m, d] = parseDate dt in Date y m d
  where parseDate dt' = map read $ splitRegex (mkRegex "/") dt'

toCurrency :: String -> Currency
toCurrency val = read $ subRegex (mkRegex "\\$") val ""

isDebit :: Currency -> Bool
isDebit val = val < 0