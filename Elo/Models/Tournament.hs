module Models.Tournament where
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, findIndex)
import SpringRank.CsvParser (Edge(Edge))
import Elo (rankToElo, eloGain, Pairing(M))
import Models.Matches (Match(Match), getMatches, n1, n2)
import Models.Player (Player(Player), idx)

example = genTournament "SpringRank/data/2018_matches.dat"

{--
Data here is given as matchings where the first player listed won the bout
against the second player listed.
--}

data Tournament = Tournament {
  player1 :: Player,
  player2 :: Player,
  eloDiff :: !Double,
  open :: !Bool
} deriving (Show)

genTournament :: FilePath -> IO [Tournament]
genTournament file = do
  matches <- getMatches file
  let names = nub $ map n1 matches ++ map n2 matches
  return $ map (toTable names) matches
  where
    toTable ns (Match n1 r1 n2 r2 open) =
      (Tournament
        (genPlayer ns n1 r1)
        (genPlayer ns n2 r2)
        (diffElo (rankToElo r1) (rankToElo r2) open)
        (read open)
      )

    -- Player idx name goRank elo tournamentRank
    -- probably should be an empty value for a newtype
    genPlayer ns n r =
      Player (playerId n ns) n r (rankToElo r) 0

    playerId n ns =
      let (Just x) = findIndex (== n) ns in x

    diffElo a b o
      | a < b = eloGain (M b a) False (read o)
      | otherwise = eloGain (M a b) True (read o)

-- generates a Tournment and then forgets into a Graph
genGraph file = do
  t <- genTournament file
  return $ map (\r -> Edge (idx.player1 $ r) (idx.player2 $ r) (eloDiff r)) t
