module Test where
import Data.List (init, nub, findIndex)
import SpringRank.SpringRank (springRank)
import SpringRank.PrintHelper (sortRank)
import Data.Bifunctor (bimap)
import Models.Tournament
import Models.Matches
import Models.Player

normalize :: [(a, Double)] -> [(a, Double)]
normalize ls = let k = 1 / (maximum.map snd $ ls) in
  [(l, k * val) | (l, val) <- ls]

getTournamentRankings file = do
  es <- springRank file
  ps <- genPlayers file
  let adjustedElo = zipWith (*) (map elo ps) (map snd es)
  let tournamentRank = zip (map name ps) adjustedElo
  return.sortRank.normalize $ tournamentRank

nmgo_2018_test = getTournamentRankings "SpringRank/data/2018_matches.dat"
nmgo_2019H_test = getTournamentRankings "SpringRank/data/2019H_matches.dat"
nmgo_2019O_test = getTournamentRankings "SpringRank/data/2019O_matches.dat"
