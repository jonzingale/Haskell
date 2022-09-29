module SpringRank.Players where
import SpringRank.CsvWriter (writeToFile)
import SpringRank.GoParser (getMatches, players_with_id)

players :: FilePath -> IO [(Int, String)]
players file = do
  mts <- getMatches file
  return $ players_with_id mts

-- writePlayers = do
--     ps <- players "SpringRank/data/2018_matches.dat"
--     writeToFile $ players_with_id ps
