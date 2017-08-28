module Vectors2Matrices where

import System.Random
import SevenVectors
import Diagonals2
import Graphables
import Sorts

targets :: [N] -> [[N]]
targets ss = [tt | tt <- seven_vectors, join ss tt]

all_targets :: [([N], Int)] -- returns distribution of targets
all_targets = sort_lasts [(vv, (length.targets) vv) | vv <- seven_vectors]

-- Given a matrix, find it's component
-- sans zero = [0,0,0,0,0,0,0]. Even
-- without zero, everyone points to
-- everyone.

vect = [1,1,0,2,0,0,1]
branches = [[0,0,0,0,0,0,1],[0,0,0,0,0,1,0],[0,0,0,0,0,1,1],[0,0,0,0,0,2,0],[0,0,0,0,1,0,0],[0,0,0,0,1,0,1],[0,0,0,0,1,1,0],[0,0,0,0,1,1,1],[0,0,0,0,2,0,0],[0,0,0,0,2,0,1],[0,0,0,0,2,2,0],[0,0,0,2,0,0,0],[0,0,0,2,0,0,1],[0,0,0,2,0,1,0],[0,0,0,2,0,1,1],[0,0,0,2,0,2,0],[0,0,0,2,2,0,0],[0,0,0,2,2,0,1],[0,0,0,2,2,2,0],[1,0,0,0,0,0,0],[1,0,0,0,0,0,1],[1,0,0,0,0,1,0],[1,0,0,0,0,1,1],[1,0,0,0,0,2,0],[1,0,0,0,1,0,0],[1,0,0,0,1,0,1],[1,0,0,0,1,1,0],[1,0,0,0,1,1,1],[1,0,0,0,2,0,0],[1,0,0,0,2,0,1],[1,0,0,0,2,2,0],[1,0,0,2,0,0,0],[1,0,0,2,0,0,1],[1,0,0,2,0,1,0],[1,0,0,2,0,1,1],[1,0,0,2,0,2,0],[1,0,0,2,2,0,0],[1,0,0,2,2,0,1],[1,0,0,2,2,2,0]]

all_nodes :: [N] -> [[N]]
all_nodes vv = f [vv] []
  where
    f [] ns = ns 
    f (v:vs) ns | length ns == (length.uniq) (targets v ++ ns) = f vs ns
                | otherwise = f (uniq (targets v ++ ns)) (uniq (targets v ++ ns))

{--
Let's get weird. importing System.Random,
it's time to walk.
--}

mkBlanket :: Int -> StdGen
mkBlanket cozy = mkStdGen cozy

ranged :: N -> Int -> [N]
ranged n seed = randomRs (0,n) $ mkBlanket seed

random_matrix s = [seven_vectors!!(fromIntegral rand) | rand <- take 7 (ranged 576 s)]

valid_rands n | processList (random_matrix n) =  countem(random_matrix n) : (valid_rands (n+1))
              | otherwise = valid_rands (n+1)

countem :: [[N]] -> Int
countem [] = 0
countem (n:ns) = (length [x | x<-n, x/=0]) + countem ns

-- 7223