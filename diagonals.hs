-- :set +s for testing run time speed
module Diagonals where
type N = Integer

{-- All cases are relative to a 7 x 7 grid
Optimizations and concerns:
Both:
* perhaps bit operators?
* pre-process with bins as there are fewer.

Boxes:
* There are 49 squares, thus 3^49 representations:
  23 * 10^22 cases to sort through.

* The total 1's and 2's must be 29

Corners:
* There are 64 corners, thus 2^64 representations:
  18 * 10^18 cases to sort through.

* The total 1's must be 58: [536870911..2^64]

--}
-- Both
nonzeros :: [N] -> N
nonzeros [] = 0
nonzeros (0:ns) = nonzeros ns
nonzeros (n:ns) = 1 + nonzeros ns

mary :: [N] -> N
mary [] = 0
mary (n:ns) = n * 10^(length ns) + mary ns

-- Boxes
base3 :: N -> [N]
base3 n = reverse.ff $ n
  where
    ff n | n < 3 = [n]
         | otherwise = mod n 3 : ff (div n 3)

-- number of combinations of vertices.
vert_choices n = 2^((n+2)^2)
-- assuming Dean is correct about 1 mod 6.
correct_verts n | mod n 6 == 1 = n^2 + n + 2
                | otherwise = n^2 + n

--write conditions on verts.
-- cond1 ::

-- Corners
base2 :: N -> [N]
base2 n = reverse.ff $ n -- only reverse when necessary.
  where
    ff n | n < 2 = [n]
         | otherwise = mod n 2 : ff (div n 2)

binary :: [N] -> N
binary [] = 0
binary (n:ns) = n * 2^(length ns) + binary ns

nonzerobin :: N -> N
nonzerobin n = foldr (+) 0 $ base2 n

-- cond1: correct number of corners covered at all.
valid_bins :: [N] -> N
valid_bins [] = 0
valid_bins (n:ns) | nonzerobin n /= 20 = valid_bins ns
                  | otherwise = 1 + valid_bins ns 