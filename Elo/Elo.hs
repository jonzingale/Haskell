module Elo where

{--
This module computes Elo and is a helper to FairTournament.hs
--}

type Elo = Float
type Rank = Float

-- Where for M a b, a < b in initial rank
-- dependent types?
data Match = M Elo Elo deriving (Show, Eq)

-- win_probability (M 400 1000) => (0.18242553,0.8175745)
win_probability :: Match -> (Float, Float)
win_probability (M a b) =
  let prob_a = 1 / (1 + exp((b-a)/a)) in
  (prob_a, 1 - prob_a)

eloDiff :: Match -> (Rank, Rank)
eloDiff match =
  let (x, y) = win_probability match in
  let f t = 32 * (1 - t) in
  (f x, f y)

diffA :: Match -> Rank
diffA = fst.eloDiff

diffB :: Match -> Rank
diffB = snd.eloDiff  

{--
Amateur ranks are effectively linear.

2600 26 6 dan
2500 25 5 dan
2400 24 4 dan
2300 23 3 dan
2200 22 2 dan
2100 21 1 dan
2000 20 1 kyu
1900 19 2 kyu
1800 18 3 kyu
1500 15 6 kyu
1000 10 11 kyu
500  5 16 kyu
100  1 20 kyu

| rank == kyu = (21 - rank) * 100
| otherwise = 100 * rank + 2000
--}
rankToElo :: String -> Elo
rankToElo r =
  case r of
    "6d" -> 2850
    "3d" -> 2300
    "2.5d" -> 2250
    "2d" -> 2200
    "3k" -> 1800
    "3.2k" -> 1780
    "3.3k" -> 1770
    "4k" -> 1700
    "5k" -> 1600
    "5.8k" -> 1520
    "6k" -> 1500
    "6.1k" -> 1490
    "6.7k" -> 1430
    "7k" -> 1400
    "7.6k" -> 1340
    "9k" -> 1200
    "20k" -> 100
    "20.5k" -> 50
