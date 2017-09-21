module Conditions where
import ZipperTree
import Traversal

grid = 5

cond :: Traversal Integer -> Bool
cond trav = or [cond1 trav, cond2 trav, cond3 trav]

cond1 :: Traversal a -> Bool -- height condition
cond1 trav = getHeight trav == grid^2 -- single node has height 1

cond2 :: Traversal Integer -> Bool -- adjacency condition
cond2 trav | mod (getHeight trav) grid == 1 = False
           | otherwise = any (== mod (getVal trav) 100) [12, 21]

cond3 :: Traversal Integer -> Bool -- neighbor condition
cond3 trav = neigh (getFocus trav)
  where
    neigh (n, h, f, p) | lst n == 0 = False
                       | div h grid == 0 = False
                       | lst n == 1 = onesCase n h
                       | lst n == 2 = twosCase n h
                       | otherwise = False

    onesCase n h | mod h grid == 1 = get7 n == 2
                 | otherwise = or [get7 n == 2, get8 n == 1]
    twosCase n h | mod h grid == 0 = get7 n == 1
                 | otherwise = or [get6 n == 2, get7 n == 1]

    get6 n = div (mod n (10^grid)) $ 10^(grid-1)
    get7 n = div (mod n (10^(grid+1))) $ 10^grid
    get8 n = div (mod n (10^(grid+2))) $ 10^(grid+1)
    lst n = mod n 10
