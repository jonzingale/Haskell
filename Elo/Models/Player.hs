{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Player where
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromRecord, ToRecord)
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import Control.Monad (liftM)
import Data.Char (ord)
import Data.List (nub)
import GHC.Generics
import Data.Csv

import Models.Matches
import Elo (rankToElo)

{--
Todo:
1. Build out Player model
2. pull writers and readers into Models
3. generate default players from raw tournament data and save it.
4. derive tournament data from springrank and update players table.
--}

example = genPlayers "SpringRank/data/2018_matches.dat"

instance FromRecord Player
instance ToRecord Player

type EitherData = Either String (Vector Player)

data Player = Player
  { idx :: !Int,
    name :: !String,
    goRank :: !String,
    elo :: !Double,
    tournamentRank :: !Double
  } deriving (Show, Eq, Generic)

-- Player 0 "Billy" "10.0k" 1100 0,
getPlayers :: IO [Player]
getPlayers =
  let file = "SpringRank/data/players.csv" in
  liftM records $ BL.readFile file
  where
    records = toList.(fromRight empty).parseCsv
    options = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }
    parseCsv csv = decodeWith options NoHeader csv :: EitherData

genPlayers :: FilePath -> IO [Player]
genPlayers file = do
  matches <- getMatches file
  let fsts = [Player 0 (n1 mt) (r1 mt) (rankToElo.r1 $ mt) 0 | mt <- matches]
  let snds = [Player 0 (n2 mt) (r2 mt) (rankToElo.r2 $ mt) 0 | mt <- matches]
  let rs = zip [0..] (nub $ fsts ++ snds)
  let ps = [ Player i n r e t | (i, (Player _ n r e t)) <- rs]
  return ps

