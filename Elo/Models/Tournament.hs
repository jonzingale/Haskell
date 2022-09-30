module Models.Tournament where
import Data.List (init, nub, findIndex)
import SpringRank.CsvParser (Edge(Edge))
import Elo (rankToElo, eloGain, Pairing(M))
import Models.Matches (Match(Match), getMatches, n1, n2)

example = genTable "SpringRank/data/2018_matches.dat"

{--
Data here is given as matchings where the first player listed won the bout
against the second player listed.

[name1, go_ranking1, name2, go_ranking2, open?]

The parser should prepare the data so that go_rankings are converted to elo,
and the gained elo, via the match, is accounted for. Additionally, each
player is to be given a unique integer id (index for the adjacency matrix).

[id1, name1, go_ranking1, elo1, id2, name2, go_ranking2, elo2, elo_gain, open?]
--}

data Table = Table {
  id1 :: !Int,
  name1 :: !String,
  rank1 :: !String,
  elo1 :: !Double,
  id2 :: !Int,
  name2 :: !String,
  rank2 :: !String,
  elo2 :: !Double,
  eloDiff :: !Double,
  open :: !Bool
} deriving (Show)

genTable :: FilePath -> IO [Table]
genTable file = do
  matches <- getMatches file
  let names = nub $ map n1 matches ++ map n2 matches
  return $ map (toTable names) matches
  where
    toTable ns (Match n1 r1 n2 r2 open) =
      (Table
        (playerId n1 ns) n1 r1 (rankToElo r1)
        (playerId n2 ns) n2 r2 (rankToElo r2)
        (diffElo (rankToElo r1) (rankToElo r2) open)
        (read open)
      )
    playerId n ns =
      let (Just x) = findIndex (== n) ns in x

    diffElo a b o
      | a < b = eloGain (M b a) False (read o)
      | otherwise = eloGain (M a b) True (read o)

genGraph file = do
  t <- genTable file
  return $ map (\r -> Edge (id1 r) (id2 r) (eloDiff r)) t
