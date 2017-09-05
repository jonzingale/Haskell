module Recurrence where

type N = Integer

choose :: N -> N -> N
choose n 0 = 1
choose n k | n < k = 0
           | otherwise = choose (n-1) (k-1) + choose (n-1) k

-- no Show. corecusion.
ff :: N -> N
ff 0 = ff 0 + ff 1
ff 1 = ff 0

fib :: N -> N
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Diagonals
dd :: N -> N -- lotka-like
dd 0 = dd 1 + dd 0 + dd 2
dd 1 = dd 1 + dd 0
dd 2 = dd 0 + dd 2