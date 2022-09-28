module Test where
import SpringRank.GoParser
import SpringRank.SpringRank (springRank)
import SpringRank.PrintHelper (sortRank)

normalize :: [(Int, Double)] -> [(Int, Double)]
normalize ls = let k = 1 / (maximum.map snd $ ls) in
  [(l, k * val) | (l, val) <- ls]

nmgo_test = do
  -- es <- springRank "data/2018_adjusted_elo.dat"
  rs <- genTable "SpringRank/data/2018_matches.dat"
  print $ rs
  es <- springRank "SpringRank/data/2018_matches.dat"
  print $ sortRank $ normalize es
