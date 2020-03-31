{-# LANGUAGE DeriveGeneric #-}

import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import GHC.Generics (Generic)
import Data.Char (ord)
import Data.Csv

file = BL.readFile "data/kirchoff.dat"

type EitherData = Either String (Vector Graph)

data Graph = Edge { source :: !Int,  target :: !Int, value :: !Int }
  deriving (Generic, Show)

instance FromRecord Graph

records = toList.(fromRight empty).parseCsv
  where
    options = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }
    parseCsv csv = decodeWith options NoHeader csv :: EitherData
