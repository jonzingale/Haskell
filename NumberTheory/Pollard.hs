-- https://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm

module Pollard where

pollard n = f 2 5 n 1
  where
    f x y n 1 = f (g x) (g y) n (gcd n $ x - g y)
    f x y n d = d
    g t = t^2 + 1

test = map pollard [1234, 300^300-1, 121]