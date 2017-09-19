module Conditions where
import ZipperTree
import Traversal

-- get this Logic right: verify neighborhood condition.
-- cond :: Traversal Integer -> Bool
-- cond trav = or [cond1 trav, cond2 trav, cond3 trav]

-- Dont use flags to calculate!!!

cond1 :: Traversal a -> Bool -- height condition
cond1 trav = getHeight trav >= 49

cond2 :: Traversal Integer -> Bool -- adjacency condition
cond2 trav = let (n, a) = divMod (getVal trav) 10 in
             let b = mod n 10 in
             a + b == 3
-- adjacency only if in same row.
-- | getHeight trav <= 7 = False

cond3 :: Traversal Integer -> Bool -- neighborhood condition
cond3 trav = or [div (getHeight trav) 7 == 0, -- first row ok.
                 neigh (getFocus trav)]

type Focus = (N, Height, Flag)
type Height = Int
type N = Integer

-- Are there any Neighborhood restrictions?
-- ERROR: Flags are not the same as last DIGIT.
neigh :: Focus -> Bool
neigh (n, h, Zero) = False -- zeros are always cool
neigh (n, h, One) | mod h 7 == 1 = get7 n == 2
                  | otherwise = or [get7 n == 2, get8 n == 1]
neigh (n, h, Two) | mod h 7 == 0 = get7 n == 1
                  | otherwise  = or [get6 n == 2, get7 n == 1]
neigh (n, h, _) = False -- let everyone else pass

get6 n = div (mod n (10^7)) $ 10^6
get7 n = div (mod n (10^8)) $ 10^7
get8 n = div (mod n (10^9)) $ 10^8
get876 n = div (mod n (10^9)) $ 10^6
lst n = mod n 10