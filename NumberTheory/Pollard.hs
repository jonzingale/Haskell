-- https://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm

module Pollard where

--different poly's (u^2+1) give different results
-- pollard = (head.polliod)
pollard n = head $ dropWhile (== 1) [ gcd (j-i `mod` n) n | (i, j) <- por n ]
por m = [ ( 2^n `mod` m,  2^(2*n) `mod` m)| n <- [1..m]]

pollardFactors 1 = []
pollardFactors n = pollard n : pollardFactors (n `div` pollard n)
