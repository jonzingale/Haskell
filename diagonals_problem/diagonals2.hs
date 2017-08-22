 -- :set +s for testing run time speed
module Diagonals2 where
type N = Integer

good_list = [[2,2,0],[0,0,1],[1,0,1]]
bad_list = [[2,2,0],[0,2,1],[1,0,1]]
infinite_bad = [[2,2,0],[0,0,1],[1,0,1]] ++ infinite_bad

top :: [N] -> N
top [] = 0
top (n:ns) | n == 2 = 1 * 10^length(ns) + top ns
           | n == 1 = 1 * 10^length(n:ns) + top ns
           | otherwise = top ns

bot :: [N] -> N
bot [] = 0
bot (n:ns) | n == 1 = 1 * 10^length(ns) + bot ns
           | n == 2 = 1 * 10^length(n:ns) + bot ns
           | otherwise = bot ns

join :: [N] -> [N] -> Bool
join ts bs = validV $ bot ts + top bs

validV :: N -> Bool
validV n | n < 2 = True
         | mod n 10 == 2 = False
         | otherwise = validV $ div n 10

vand :: Bool -> Bool -> Bool
vand n m = and [n, m]

processList :: [[N]] -> Bool
processList [] = True
processList (n:m:[]) = join n m
processList (n:m:ps) = vand (join n m) $ processList (m:ps)
