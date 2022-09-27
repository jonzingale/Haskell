module Elo where

{--
This module computes Elo and is a helper to FairTournament.hs
--}

type Elo = Float
type Rank = Float

-- Where for M a b, a > b in initial rank. Dependent types?
data Pairing = M Elo Elo deriving (Show, Eq)

-- win_probability (M 1000 400) => (0.8175745, 0.18242553)
win_probability :: Pairing -> (Float, Float)
win_probability (M a b) =
  let prob_b = 1 / (1 + exp((a-b)/b)) in
  (1 - prob_b, prob_b)

-- eloDiff (M 1000 400) => (5.837616, 26.162384)
eloDiff :: Pairing -> (Rank, Rank)
eloDiff match =
  let (x, y) = win_probability match in
  let f t = 32 * (1 - t) in
  (f x, f y)

-- Pairing -> Expected Player Wins? -> Open? -> Elo
eloGain :: Pairing -> Bool -> Bool -> Elo
eloGain mt True True = fst.eloDiff $ mt -- stronger player wins
eloGain mt False True = snd.eloDiff $ mt -- weaker player wins
eloGain mt _ False = fst.eloDiff $ (M 1 1) -- handicap case

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
--}

rankToElo :: String -> Float
rankToElo rank
  | last rank == 'k' = (21 - toRank rank) * 100
  | otherwise = 100 * (toRank rank) + 2000
  where toRank r = read.init $ r
