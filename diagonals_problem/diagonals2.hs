-- :set +s for testing run time speed
module Diagonals2 where
type N = Integer

jt = [2,2,0]
test1 = (itop jt, ibot jt)

itop, ibot :: [N] -> [N]
itop ns = ns ++ [0]
ibot ns = 0 : ns

-- top
-- 0 -> [0,0]
-- 1 -> [1,0]
-- 2 -> [0,1]

-- z2top :: [N] -> [N]
-- z2top [] = []
-- z2top (x:y:zs) | x == 1 = 1 : z2top (y:zs)
               -- | x == 2 = 
               -- | otherwise = 0 : z2top (y:zs)