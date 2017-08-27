 -- :set +s for testing run time speed
module Diagonals2 where
import System.Random
import SevenVectors
import DynDistr
import Sorts

{--
seven_vectors, qsort, sort_lasts
target_distributions, count_distr
--}

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

targets :: [N] -> [[N]]
targets ss = [tt | tt <- seven_vectors, join ss tt]

all_targets :: [([Integer], Int)] -- returns distribution of targets
all_targets = sort_lasts [(vv, (length.targets) vv) | vv <- seven_vectors]

-----All 7 x 7s
{--
To count maxima, covered_corners/2.
3^7 Vectors thus (3^7)^7 == 3^49
239299329230617529590083 matrices.

still only 3^7 = 2187 vectors so,
how many less are valid?
577/2187 = 0.2638317329675354
21292697885552828353 matrices, 577^7.
still, which of these are valid?
--}

vects n = map tern [0..3^n-1] -- only n digit numbers.
goodVs n = [vv | vv <- vects n, validV vv]

-- [3,7,17,41,99,239,577,1393,3363,8119,19601,47321,114243
-- on remark, every 4th is divisible by 3. why?
vs_sub_k = [length.goodVs $ k | k<-[1..20]]

tern :: N -> N
tern n = f n 0
  where
    f n i | n < 3 = n * 10^i
          | otherwise = mod n 3 * 10^i + f (div n 3) (i+1)

validV :: N -> Bool
validV n | n < 10 = True
         | sum_tail n == 3 = False
         | otherwise = validV.div n $ 10

sum_tail :: N -> N
sum_tail n = uncurry (+) $ divMod (mod n 100) 10

vectorize :: N -> [N]
vectorize n | div n 10 == 0 = [n]
            | otherwise = vectorize (div n 10) ++ [mod n 10]

sevenpad :: [N] -> [N]
sevenpad ns | length ns > 6 = ns
            | otherwise = sevenpad (0:ns)

all_sevens = map (sevenpad.vectorize) $ goodVs 7

{--
Here I should perhaps think about file systems
and random matrices or something. The idea is
to eventually define a poisson process or a
Birthdays distribution so randomly search the
space of all matrices. When valid matrices are
found, store them in a file.
--}
