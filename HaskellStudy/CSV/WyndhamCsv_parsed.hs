
{-# LANGUAGE DeriveGeneric #-}

module WyndhamCsv where
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import GHC.Generics (Generic)
import Data.List
import Data.Csv

type EitherGoogle = Either String (Vector GoogleRecord)
type EitherWyndham = Either String (Vector WyndhamRecord)

data GoogleRecord = BadGoogleRecord |
                    GRec {accountId :: !Integer,
                          accountName :: !String,
                          groupName :: !String,
                          hotelId :: !Integer,
                          hotelName :: !String} deriving (Generic, Show)

data WyndhamRecord = WRec {brand :: !String, site :: !Integer} |
                     BadWyndhamRecord deriving (Generic, Show)

instance FromRecord GoogleRecord
instance FromRecord WyndhamRecord where -- namedRecord would be more specific.
    parseRecord v = WRec <$> v .! 0 <*> v .! 2 -- 0th and 2nd index in Csv

googleRecords = toList.(fromRight empty).parseCsv
  where parseCsv csv = decode HasHeader csv :: EitherGoogle

wyndhamRecords = toList.(fromRight empty).parseCsv
  where parseCsv csv = decode HasHeader csv :: EitherWyndham

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
  google <- BL.readFile "google.csv"
  wyndham <- BL.readFile "wyndham.csv"
  let grecords = googleRecords google
  let wrecords = wyndhamRecords wyndham
  putStrLn.show $ returnBrand grecords wrecords
