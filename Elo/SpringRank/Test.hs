module Test where
import SpringRank (springRank)
import PrintHelper (sortRank)

normalize :: [(Int, Double)] -> [(Int, Double)]
normalize ls = let k = 1 / (maximum.map snd $ ls) in
  [(l, k * val) | (l, val) <- ls]

nmgo_test = do
  es <- springRank "data/2018_adjusted_elo.dat"
  print $ sortRank $ normalize es
