module Conditions where
import ZipperTree
import Traversal

-- Validity conditions for diagonal trees
-- cond :: Traversal Integer -> Bool
-- cond trav = or [cond1 trav, cond2 trav, cond3 trav]

type Focus = (N, Height, Flag)
type Height = Int
type N = Integer

testZip = (freeTree, [])
cond1test = cond1 (list2tree [0 | i<-[1..48]] testZip)
cond2test = cond2 (list2tree [1,0,2,0,1,1,2] testZip)

cond1 :: Traversal a -> Bool -- height condition
cond1 trav = getHeight trav >= 49 -- single node has height 1

cond2 :: Traversal Integer -> Bool -- adjacency condition
cond2 trav | mod (getHeight trav) 7 == 1 = False
           | otherwise =
             let (n, a) = divMod (getVal trav) 10 in
             let b = mod n 10 in
             a + b == 3

cond3 :: Traversal Integer -> Bool -- neighbor condition
cond3 trav = neigh (getFocus trav)

--perhaps rewrite this with clearer
--guard cases. Also for generalizing
--to arbritrary grid sizes.
neigh :: Focus -> Bool
neigh (n, h, f) | lst n == 0 = False
                | div h 7 == 0 = False
                | and [lst n == 1, mod h 7 == 1] = get7 n==2
                | and [lst n == 1, mod h 7 /= 1] = or [get7 n == 2, get8 n == 1]
                | and [lst n == 2, mod h 7 == 0] = get7 n == 1
                | and [lst n == 2, mod h 7 /= 0] = or [get6 n == 2, get7 n == 1]
                | otherwise = False

neigh2 :: Focus -> Bool
neigh2 (n, h, f) | lst n == 0 = False
                 | div h 7 == 0 = False
                 | lst n == 1 = onesCond (n, h)
                 | lst n == 2 = twosCond (n, h)
                 | otherwise = False

onesCond (n, h) | mod h 7 == 1 = get7 n == 2
                | otherwise = or [get7 n == 2, get8 n == 1]
twosCond (n, h) | mod h 7 == 0 = get7 n == 1
                | otherwise = or [get6 n == 2, get7 n == 1]


get6 n = div (mod n (10^7)) $ 10^6
get7 n = div (mod n (10^8)) $ 10^7
get8 n = div (mod n (10^9)) $ 10^8
-- get876 n = div (mod n (10^9)) $ 10^6
lst n = mod n 10

