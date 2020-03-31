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

data Edge = BadEdge | Edge { source :: !Int,  target :: !Int, value :: !Double }
  deriving (Generic, Show)

instance Eq Edge where
  (Edge s t _) == (Edge s' t' _) = s == s' && t == t'

instance Monoid Edge where
  mempty = Edge 0 0 0
  mappend (Edge a b v) (Edge c d w)
    | (Edge a b v) == mempty = (Edge c d w)
    | (Edge c d w) == mempty = (Edge a b v)
    | a == c && b == d = Edge a b (v + w)
    | otherwise = BadEdge

instance FromRecord Edge

records = toList.(fromRight empty).parseCsv
  where
    options = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }
    parseCsv csv = decodeWith options NoHeader csv :: EitherData

kirchoff = do
  file <- BL.readFile "data/kirchoff.dat"
  return $ records file