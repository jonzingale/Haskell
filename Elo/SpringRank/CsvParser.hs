{-# LANGUAGE DeriveGeneric #-}

module SpringRank.CsvParser where

import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import GHC.Generics (Generic)
import Control.Monad (liftM)
import Data.Char (ord)
import Data.Csv

type Graph = [Edge]
type EitherData = Either String (Vector Edge)

-- Edge Id Id DiffElo
data Edge = BadEdge | Edge { source :: !Int,  target :: !Int, value :: !Double }
  deriving (Generic, Show)

-- edges are oriented
instance Eq Edge where
  (Edge s t _) == (Edge s' t' _) = s == s' && t == t'

instance Semigroup Edge where
  (<>) (Edge a b v) (Edge c d w)
    | (Edge a b v) == mempty = (Edge c d w)
    | (Edge c d w) == mempty = (Edge a b v)
    | a == c && b == d = Edge a b (v + w)
    | otherwise = BadEdge

instance Monoid Edge where
  mempty = Edge 0 0 0

instance FromRecord Edge

records = toList.(fromRight empty).parseCsv
  where
    options = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }
    parseCsv csv = decodeWith options NoHeader csv :: EitherData

getGraph :: FilePath -> IO(Graph)
getGraph file = liftM records $ BL.readFile file
