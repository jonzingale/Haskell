module Test where
import Data.List (init, nub, findIndex)
import SpringRank.SpringRank (springRank)
import SpringRank.PrintHelper (sortRank)
import Models.Tournament
import Models.Matches

normalize :: [(Int, Double)] -> [(Int, Double)]
normalize ls = let k = 1 / (maximum.map snd $ ls) in
  [(l, k * val) | (l, val) <- ls]

players_with_id :: [Match] -> [(Int, String)]
players_with_id mts =
  let names = nub $ map n1 mts ++ map n2 mts in
  zip [0..] names

players :: FilePath -> IO [(Int, String)]
players file = do
  mts <- getMatches file
  return $ players_with_id mts

nmgo_test = do
  -- es <- springRank "data/2018_adjusted_elo.dat"
  -- rs <- genTable "SpringRank/data/2018_matches.dat"
  -- print $ rs
  ps <- players "SpringRank/data/2018_matches.dat"
  print ps
  es <- springRank "SpringRank/data/2018_matches.dat"
  print $ sortRank $ normalize es
