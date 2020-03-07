{-# LANGUAGE DeriveGeneric #-}

module BankParser where

import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import GHC.Generics (Generic)
import Data.Csv

type EitherBank = Either String (Vector BankRecord)

type Date = String
type Description = String
type Money = Float

data BankRecord =
  BadBankRecord |
  GCU { date :: !Date,
        description :: !Description,
        debit :: !String,
        credit :: !String }
        deriving (Generic, Show)

instance FromRecord BankRecord

bankRecords = toList.(fromRight empty).parseCsv
  where parseCsv csv = decode NoHeader csv :: EitherBank

moneyToFloat :: String -> Money
moneyToFloat (s:t:rs) | s == '-' = read (s:rs)
                      | otherwise = read (t:rs)

main = do
  oneYear <- BL.readFile "./one_year.csv"
  let statements = bankRecords oneYear
  let credits = map (moneyToFloat.credit) statements
  print credits

