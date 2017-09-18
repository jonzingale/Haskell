module DigitalString where

{-- Motivation
* Define a tree traversal, starting from the
left and working its way up and to the right.
* Write bounding functions:
  - only 49 deep
  - adjacency rules
  - CA like rules
* Return valid strings.

Where to start:
Extend the data structure of the Zipper to
include height and index data. perhaps height
can be embedded directly into the Node information.

upperNeigh :: N -> (N,N,N) -> Bool
upperNeigh 0 _ = True
upperNeigh 1 ns = any (== ns) $ both ++ [(0,0,2), (0,1,0), (0,1,1), (2,0,2)]
upperNeigh 2 ns = any (== ns) $ both ++ [(0,2,0), (1,0,0), (1,0,1), (2,2,0)]
both = [(0,0,0), (0,0,1), (2,0,0), (2,0,1)]
--}

data Flag = One | Zero | Two | Full deriving (Eq, Show)
type Focus = (N, Height, Flag)
type Height = Int
type N = Integer

good, bad, firstOne, lastOne :: Focus
good = (10222010102, 11, Two) -- is actually bad by above
bad  = (10222012, 8, Two) -- not yet because of above.
firstOne = (10222012, 8, Two)
lastOne = (10222010102001, 14, Two)

len :: N -> N
len n | n < 10 = 1
      | otherwise = 1 + (len.div n) 10

last14 :: N -> N
last14 n = mod n $ 10^(len n - 7)

-- Are there any Neighborhood restrictions?
neigh :: Focus -> Bool
neigh (n, h, f) | f == Zero = False
                | div h 7 == 0 = False -- any first row is ok
                | and [f == One, mod h 7 == 1] = False -- first place 1 is ok
                | and [f == One, mod h 7 == 0] = smallN n -- last 1 
                | and [f == Two, mod h 7 == 0] = False
                | and [f == Two, mod h 7 == 1] = smallN n -- first 1
                | otherwise = True

smallN :: Integer -> Bool -- here is is assumed h > 7
smallN n = any (== get76 n) [10, 2, 12]
    

num = 9876543210

nth :: N -> N -> N
nth i n = div (mod n (10^(i+1))) $ 10^i

get76 n = div (mod n (10^8)) $ 10^6
get876 n = div (mod n (10^9)) $ 10^6

upperNeigh :: N -> (N,N,N) -> Bool
upperNeigh 0 _ = True
upperNeigh 1 ns = any (== ns) $ both ++ [(0,0,2), (0,1,0), (0,1,1), (2,0,2)]
upperNeigh 2 ns = any (== ns) $ both ++ [(0,2,0), (1,0,0), (1,0,1), (2,2,0)]
both = [(0,0,0), (0,0,1), (2,0,0), (2,0,1)]

