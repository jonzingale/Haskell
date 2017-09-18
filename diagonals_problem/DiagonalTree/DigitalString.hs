module DigitalString where

-- Motivation
-- upperNeigh :: N -> (N,N,N) -> Bool
-- upperNeigh 0 _ = True
-- upperNeigh 1 ns = any (== ns) $ both ++ [(0,0,2), (0,1,0), (0,1,1), (2,0,2)]
-- upperNeigh 2 ns = any (== ns) $ both ++ [(0,2,0), (1,0,0), (1,0,1), (2,2,0)]
-- both = [(0,0,0), (0,0,1), (2,0,0), (2,0,1)]

data Flag = One | Zero | Two | Full deriving (Eq, Show)
type Focus = (Integer, Height, Flag)
type N = Integer
type Height = Int

good, bad :: Focus
good = (10222010102, 11, Two) -- is actually bad by above
bad  = (10222012, 8, Two) -- not yet because of above.

len :: N -> N
len n | n < 10 = 1
      | otherwise = 1 + (len.div n) 10

last14 :: N -> N
last14 n = mod n $ 10^(len n - 7)

-- Are there any Neighborhood restrictions?
neigh :: Focus -> Bool
neigh (n, h, f) | f == Zero = False
                | div h 7 == 0 = False
                | and [f == One, mod h 7 == 1] = False
                | and [f == One, mod h 7 == 0] = smallN (n, h, f)
                | and [f == Two, mod h 7 == 0] = False
                | and [f == Two, mod h 7 == 1] = smallN (n, h, f)
                | otherwise = True

smallN :: Focus -> Bool -- here is is assumed h > 7
smallN (n, h, f) = 