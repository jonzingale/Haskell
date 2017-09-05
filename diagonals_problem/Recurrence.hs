module Recurrence where

type N = Integer

choose :: N -> N -> N
choose n 0 = 1
choose n k | n < k = 0
           | otherwise = choose (n-1) (k-1) + choose (n-1) k

-- no Show.
ff :: N -> N
ff 0 = ff 0 + ff 1
ff 1 = ff 0