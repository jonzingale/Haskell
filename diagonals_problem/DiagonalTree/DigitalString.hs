module DigitalString where

{-- Motivation
* Define a tree traversal, starting from the
left and working its way up and to the right.
* Write bounding functions:
  - only 49 deep
  - adjacency rules
  - CA like rules
* Return valid string
--}

data Flag = One | Zero | Two | Full deriving (Eq, Show)
type Focus = (N, Height, Flag)
type Height = Int
type N = Integer

num = 9876543210

good, bad, firstOne, lastOne :: Focus
good = (10222010102, 11, Two) -- true
bad  = (10222012, 8, Two) -- true
firstOne = (10222012, 8, Two) -- true
lastOne = (10222010102001, 14, One) -- false

test = map neigh [good, bad, firstOne, lastOne]

firstRow :: Focus -> Bool
firstRow (n, h, f) = div h 7 /= 0 -- any first row is ok

-- Are there any Neighborhood restrictions?
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
