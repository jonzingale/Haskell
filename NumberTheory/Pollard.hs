-- https://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm

module Pollard where

number = 300^300-1

pollard n = f 2 2 1 n
  where
    epGcd x y n = gcd n $ abs $ g x - (g.g) y
    f x y 1 n = f (g x) (g.g $ y) (epGcd x y n) n
    f x y d n = d
    g t = t^2 + 1