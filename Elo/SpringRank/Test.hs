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

nmgo_test = do
  es <- springRank "SpringRank/data/2018_matches.dat"
  ps <- genPlayers "SpringRank/data/2018_matches.dat"

  let adjustedElo = zipWith (*) (map elo ps) (map snd es)
  let tournamentRank = zip (map name ps) adjustedElo

  print.sortRank $ tournamentRank
