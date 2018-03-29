module HotelAssignment where

import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import Data.List
import qualified Data.Csv as CSV

type Record = [String]

data GoogleRecord = GRec {accountId::Integer,
                       accountName::String,
                       groupName::String,
                       hotelId::Integer,
                       hotelName::String} | BadGoogleRecord deriving Show

data WyndhamRecord = WRec {brand::String, site::Integer} | BadWyndhamRecord
                       deriving Show

parseCsv csv = CSV.decode CSV.HasHeader csv :: Either String (Vector Record)

toGoogleRecord :: Record -> GoogleRecord
toGoogleRecord [ai, an, gn, hi, hn] = GRec (read ai) an gn (read hi) hn
toGoogleRecord _ = BadGoogleRecord

gRecords = (map toGoogleRecord).toList.(fromRight empty).parseCsv

toWyndhamRecord :: Record -> WyndhamRecord
toWyndhamRecord (brand:_:site:ts) = WRec brand (read site)
toWyndhamRecord _ = BadWyndhamRecord

wynRecords = (map toWyndhamRecord).toList.(fromRight empty).parseCsv

{--
Below function matches the hotelIds from the Google CSV to the site
from the Wyndham CSV and returns the groupName from the GoogleRecord and
the brand from the WyndhamRecord in a list of pairs.
--}
type Brand = String
type GroupName = String
returnBrand :: [GoogleRecord] -> [WyndhamRecord] -> [(GroupName, [Brand])]
returnBrand [] _ = []
returnBrand (g:gs) ws = (groupName g, f g ws) : returnBrand gs ws
    where
        f g = (map brand).filter (\w -> hotelId g == site w)

{--
google <- BL.readFile "google.csv"
wyndham <- BL.readFile "wyndham.csv"
ws = wynRecords wyndham
gs = gRecords google
--}