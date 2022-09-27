{-# LANGUAGE DeriveGeneric #-}

module GoParser where

import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import GHC.Generics (Generic)
import Control.Monad (liftM)
import Data.Char (ord)
import Data.Csv

{--
Data here is given as matchings where the first player listed won the bout
against the second player listed.

[name1, go_ranking1, name2, go_ranking2]

The parser should prepare the data so that go_rankings are converted to elo,
and the gained elo, via the match, is accounted for. Additionally, each
player is to be given a unique integer id (index for the adjacency matrix).

[id1, name1, go_ranking1, elo1, id2, name2, go_ranking2, elo2, elo_gain]
--}

type Graph = [Edge]
type EitherData = Either String (Vector Edge)

data Edge = BadEdge | Edge { source :: !Int,  target :: !Int, value :: !Double }
  deriving (Generic, Show)

data Match = BadRecord | Match {
  name1 :: !String,
  rank1 :: !String,
  name2 :: !String,
  rank2 :: !String
} deriving (Generic, Show)

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
instance FromRecord Match

records = toList.(fromRight empty).parseCsv
  where
    options = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }
    parseCsv csv = decodeWith options NoHeader csv :: EitherData

matchRecords = toList.(fromRight empty).parseCsv
  where
    options = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }
    parseCsv csv = decodeWith options NoHeader csv :: EitherData

getGraph :: FilePath -> IO(Graph)
getGraph file = liftM records $ BL.readFile file

getMatches :: FilePath -> IO(Graph)
getMatches file = liftM matchRecords $ BL.readFile file