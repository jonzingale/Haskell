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

valid_rands n | and[processList (random_matrix n), countem(random_matrix n) > 25] =
                (random_matrix n, countem(random_matrix n), n) : (valid_rands (n+1))
              | otherwise = valid_rands (n+1)

countem :: [[N]] -> Int
countem [] = 0
countem (n:ns) = (length [x | x <- n, x /= 0]) + countem ns

-- 166869588 but days ago.

{--
Let's try attack these in order
until a better idea comes along ;)

Some assumptions, some founded.
*solutions beginning with 1 are
mirrored by those beginning with 2.
*

--}

good_matrices mt = and [countem mt > 27, processList mt]
non_zeros = tail seven_vectors

all_valids = [[a, b, c, d, e, f, g] | a<-reduced_seven_vectors, b<-non_zeros,
                                      c<-non_zeros, d<-non_zeros,
                                      e<-non_zeros, f<-non_zeros,
                                      g<-non_zeros,
                                      good_matrices [a, b, c, d, e, f, g] ]









