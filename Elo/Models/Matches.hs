{-# LANGUAGE DeriveGeneric #-}

module Models.Matches where
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import GHC.Generics (Generic)
import Control.Monad (liftM)
import Data.Char (ord)
import Data.Csv

example = getMatches "SpringRank/data/2018_matches.dat"

instance FromRecord Match

type Tournament = [Match]
type EitherData = Either String (Vector Match)

data Match = BadRecord | Match {
  n1 :: !String,
  r1 :: !String,
  n2 :: !String,
  r2 :: !String,
  o1 :: !String
} deriving (Generic, Show)

getMatches :: FilePath -> IO(Tournament)
getMatches file = liftM records $ BL.readFile file
  where
    records = toList.(fromRight empty).parseCsv
    options = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }
    parseCsv csv = decodeWith options NoHeader csv :: EitherData
