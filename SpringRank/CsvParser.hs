{-# LANGUAGE DeriveGeneric #-}

module CsvParser where

import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import GHC.Generics (Generic)
import Data.Char (ord)
import Data.Csv

type Graph = [Edge]
type EitherData = Either String (Vector Edge)

data Edge = Edge { source :: !Int,  target :: !Int, value :: !Double }
  deriving (Generic, Show)

instance Eq Edge where
  (Edge s t _) == (Edge s' t' _) = s == s' && t == t'

instance Monoid Edge where
  mappend (Edge a b v) (Edge _ _ w) = Edge a b (v + w) -- Dangerous
  mempty = Edge 0 0 0

instance FromRecord Edge

records = toList.(fromRight empty).parseCsv
  where
    options = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }
    parseCsv csv = decodeWith options NoHeader csv :: EitherData

kirchoff = do
  file <- BL.readFile "data/kirchoff.dat"
  return $ records file