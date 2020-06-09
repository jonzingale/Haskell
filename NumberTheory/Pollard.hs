-- https://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm

module Pollard where

pollard n = f 1 (g 1) n
  where
    f x y n | gcd n (x - y) == 1 = f (g x) (gg y) n
            | otherwise = gcd n (x - y)
    g t = t^2 + 1
    gg = g.g

test1 = map pollard [1234, 300^300-1, 121]
test2 = take 95 $ map pollard [2..]