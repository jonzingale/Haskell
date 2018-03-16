module WyndhamCsv where

import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import qualified Data.Csv as CSV
import Data.List

type Record = [String]

data GoogleRecord = BadGoogleRecord |
                    GRec {accountId::Integer,
                          accountName::String,
                          groupName::String,
                          hotelId::Integer,
                          hotelName::String} deriving Show

data WyndhamRecord = WRec {brand::String, site::Integer} |
                     BadWyndhamRecord deriving Show

toGoogleRecord :: Record -> GoogleRecord
toGoogleRecord [ai, an, gn, hi, hn] = GRec (read ai) an gn (read hi) hn
toGoogleRecord _ = BadGoogleRecord

gRecords = (map toGoogleRecord) .toList.(fromRight empty).parseCsv
wRecords = (map toWyndhamRecord).toList.(fromRight empty).parseCsv

toWyndhamRecord :: Record -> WyndhamRecord
toWyndhamRecord (brand:_:site:_) = WRec brand (read site)
toWyndhamRecord _ = BadWyndhamRecord

parseCsv csv = CSV.decode CSV.HasHeader csv :: Either String (Vector Record)

-- hotelId -> site
type Brand = String
type GroupName = String
{-- 
This method matches each google record to each wyndham record by
matching hotelId on the first to site on the second. Then it returns 
pairs of google groupNames and wyndham brands.
--}
returnBrand :: [GoogleRecord] -> [WyndhamRecord] -> [(GroupName, [Brand])]
returnBrand [] _ = []
returnBrand (g:gs) ws = (groupName g, brands g ws) : returnBrand gs ws
  where
    brands grec = (map brand).filter (\w -> hotelId grec == site w)

-- Todo: Figure out how to do this part.
{--
google <- BL.readFile "google.csv"
wyndham <- BL.readFile "wyndham.csv"
grecs = gRecords google
wrecs = wRecords wyndham
returnBrand grecs wrecs
--}
