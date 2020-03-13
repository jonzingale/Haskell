{-# LANGUAGE DeriveGeneric #-}

module BankParser where

import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import GHC.Generics (Generic)
import Data.Csv

type EitherBank = Either String (Vector BankRecord)

data BankRecord =
  BadBankRecord |
  GCU { date :: !String,
        description :: !String,
        debit_credit :: !String,
        balance :: !String }
        deriving (Generic, Show)

instance FromRecord BankRecord

bankRecords = toList.(fromRight empty).parseCsv
  where parseCsv csv = decode NoHeader csv :: EitherBank

{-- 
  TODO:
  Write function which concatenates statements correctly via dates
--}

example header = do
  oneYear <- BL.readFile "data/one_year.csv"
  let statements = bankRecords oneYear
  let column = map header statements
  print column

