{-# LANGUAGE DeriveGeneric #-}

module CSVParser where

import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import GHC.Generics (Generic)
import Data.List
import Data.Csv


type EitherGoogle  = Either String (Vector GoogleRecord)
type EitherWyndham = Either String (Vector WyndhamRecord)

type Brand = String
type GroupName = String
type Site = Integer


data GoogleRecord = BadGoogleRecord |
                    GRec {accountId :: !Integer,
                          accountName :: !String,
                          groupName :: !String,
                          hotelId :: !Integer,
                          hotelName :: !String} deriving (Generic, Show)

data WyndhamRecord = BadWyndhamRecord | 
                     WRec {brand :: !String, site :: !Integer} deriving (Generic, Show)


instance FromRecord GoogleRecord
instance FromRecord WyndhamRecord where -- namedRecord would be more specific.
    parseRecord v = WRec <$> v .! 0 <*> v .! 2 -- 0th and 2nd index in Csv


googleRecords = toList.(fromRight empty).parseCsv
  where parseCsv csv = decode NoHeader csv :: EitherGoogle

wyndhamRecords = toList.(fromRight empty).parseCsv
  where parseCsv csv = decode NoHeader csv :: EitherWyndham