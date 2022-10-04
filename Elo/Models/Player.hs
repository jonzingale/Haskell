{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Player where
import Data.Csv (FromRecord)
import Data.List (nub)
import GHC.Generics

import Models.Matches
import Elo (rankToElo)

example = genPlayers "SpringRank/data/2018_matches.dat"

instance FromRecord Player

data Player = Player
  { idx :: !Int,
    name :: !String,
    goRank :: !String,
    elo :: !Double,
    tournamentRank :: !Double
  } deriving (Show, Eq, Generic)

genPlayers :: FilePath -> IO [Player]
genPlayers file = do
  matches <- getMatches file
  let fsts = [Player 0 (n1 mt) (r1 mt) (rankToElo.r1 $ mt) 0 | mt <- matches]
  let snds = [Player 0 (n2 mt) (r2 mt) (rankToElo.r2 $ mt) 0 | mt <- matches]
  let rs = zip [0..] (nub $ fsts ++ snds)
  let ps = [ Player i n r e t | (i, (Player _ n r e t)) <- rs]
  return ps

