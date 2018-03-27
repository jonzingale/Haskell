-- Here I write my own parsers for each Csv type.
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

instance FromRecord GoogleRecord

toGoogleRecord :: Record -> GoogleRecord
toGoogleRecord [ai, an, gn, hi, hn] = GRec (read ai) an gn (read hi) hn
toGoogleRecord _ = BadGoogleRecord

toWyndhamRecord :: Record -> WyndhamRecord
toWyndhamRecord (brand:_:site:_) = WRec brand (read site)
toWyndhamRecord _ = BadWyndhamRecord

getRecords fromRecord = (map fromRecord).toList.(fromRight empty).parseCsv
parseCsv csv = decode HasHeader csv :: Either String (Vector Record)

-- hotelId -> site
type Brand = String
type GroupName = String
{-- 
This method matches each google record to each wyndham record by
matching hotelId on the first to site on the second. Then it returns 
pairs of google groupNames with wyndham brands.
--}
returnBrand :: [GoogleRecord] -> [WyndhamRecord] -> [(GroupName, [Brand])]
returnBrand [] _ = []
returnBrand (g:gs) ws = (groupName g, brands g ws) : returnBrand gs ws
  where
    brands grec = (map brand).filter (\w -> hotelId grec == site w)

main = do  
  google  <- BL.readFile "google.csv"
  wyndham <- BL.readFile "wyndham.csv"
  let grecord = getRecords toGoogleRecord google
  let wrecord = getRecords toWyndhamRecord wyndham
  putStrLn.show $ returnBrand grecord wrecord

