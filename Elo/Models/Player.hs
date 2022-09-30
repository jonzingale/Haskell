{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Player where
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromRecord, ToRecord)
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import Control.Monad (liftM)
import Data.Char (ord)
import GHC.Generics
import Data.Csv

{--
Todo:
1. Build out Player model
2. pull writers and readers into Models
3. generate default players from raw tournament data and save it.
4. derive tournament data from springrank and update players table.
--}

instance FromRecord Player
instance ToRecord Player

type EitherData = Either String (Vector Player)

data Player = Player
  { idx :: !Int,
    name :: !ByteString,
    goRank :: !ByteString,
    elo :: !Double,
    tournamentRank :: !Double
  } deriving (Show, Eq, Generic)

persons :: [Player]
persons = [
  Player 0 "Billy" "10.0k" 1100 0,
  Player 1 "Kyle" "9.0k" 1200 0
  ]

getPlayers :: IO ([Player])
getPlayers =
  let file = "SpringRank/data/players.csv" in
  liftM records $ BL.readFile file
  where
    records = toList.(fromRight empty).parseCsv
    options = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }
    parseCsv csv = decodeWith options NoHeader csv :: EitherData

-- TODO: This won't be able to free generate the data structure. think.
-- importPlayers "SpringRank/data/2018_matches.dat"
importPlayers :: FilePath -> IO ([Player])
importPlayers file =
  liftM records $ BL.readFile file
  where
    records = toList.(fromRight empty).parseCsv
    options = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }
    parseCsv csv = decodeWith options NoHeader csv :: EitherData
