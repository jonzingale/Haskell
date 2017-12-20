module Junk where


-- (sum [sum (spiral 2), sum (spiral 4), sum (spiral 6), sum (spiral 8)]) - 3

ls n = [n, n, n]

val = 5*10^6

spiral :: Integer -> Integer
spiral n = ff n 1 val
  where
    ff n a 0 = 1
    ff n a c = a + n + ff (n + 8) (a + n) (c - 1)

spiral2 :: Integer -> Integer
spiral2 n = ff n [1] val
  where
    ff n as 0 = 1
    ff n (a:as) c = a + n + ff (n + 8) ((a + n):(a:as)) (c - 1)


totals = sum (map (spiral) [2,4,6,8]) - 3