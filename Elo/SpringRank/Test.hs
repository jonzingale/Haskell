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

nmgo_2018_test = do
  let datum = "SpringRank/data/2018_matches.dat"
  es <- springRank datum
  ps <- genPlayers datum

  let adjustedElo = zipWith (*) (map elo ps) (map snd es)
  let tournamentRank = zip (map name ps) adjustedElo

  return.sortRank.normalize $ tournamentRank

nmgo_2019H_test = do
  let datum = "SpringRank/data/2019H_matches.dat"
  es <- springRank datum
  ps <- genPlayers datum

  let adjustedElo = zipWith (*) (map elo ps) (map snd es)
  let tournamentRank = zip (map name ps) adjustedElo

  return.sortRank.normalize $ tournamentRank

nmgo_2019O_test = do
  let datum = "SpringRank/data/2019O_matches.dat"
  es <- springRank datum
  ps <- genPlayers datum

  let adjustedElo = zipWith (*) (map elo ps) (map snd es)
  let tournamentRank = zip (map name ps) adjustedElo

  return.sortRank.normalize $ tournamentRank
