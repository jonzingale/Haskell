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
join ts bs = valid_pairing $ bot ts + top bs

valid_pairing :: N -> Bool
valid_pairing n | n < 2 = True
         | mod n 10 == 2 = False
         | otherwise = valid_pairing $ div n 10

vand :: Bool -> Bool -> Bool
vand n m = and [n, m]

processList :: [[N]] -> Bool
processList [] = True
processList (n:m:[]) = join n m
processList (n:m:ps) = vand (join n m) $ processList (m:ps)


-----All 7 x 7s
{--
To count maxima, covered_corners/2.
3^7 Vectors thus (3^7)^7 == 3^49
239299329230617529590083 matrices.

still only 3^7 = 2187 vectors so,
how many less are valid?
--}

jt = (vects 7)!!42

vects n = map tern [0..3^n-1] -- only n digit numbers.

goodVs n = [vv | vv <- vects n, validV vv] -- 255 == 2^8 - 1

tern :: N -> N
tern n = f n 0
  where
    f n i | n < 3 = n * 10^i
          | otherwise = mod n 3 * 10^i + f (div n 3) (i+1)

validV :: N -> Bool
validV n | n < 10 = True
         | mod (mod n 100) 3 == 0 = False 
         | otherwise = validV.div n $ 10












-- proof:
vs_sub_k = [length.goodVs $ k | k<-[1..30]]



