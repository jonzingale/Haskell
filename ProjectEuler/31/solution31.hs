
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Solution where

{--
In England the currency is made up of pound, £, and pence, p, 
and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?
--}

euler31 = genSol uk 200
usCur31 = genSol us 100

instance Num a => Num [a] where
  fromInteger n = [fromInteger n]
  (x:xs) + (y:ys) = (x + y) : (xs + ys)
  xs + [] = xs
  [] + ys = ys
  (x:xs) * (y:ys) = (x*y) : ([x] * ys + xs * (y:ys))
  _ * _ = []

us, uk :: [Int]
us = [1,5,10,25,50,100]
uk = [1,2,5,10,20,50,100,200]

cycles bound n = [ f k n | k <- [0..bound]]
  where
    f j m | mod j m == 0 = 1
          | otherwise = 0

genSol c n = (!!n).product $ map (cycles n) c
us31Correct = (!!101).product $ map (cycles 100) us

{-- 
I suspect the problem is off by one, 201st not 200th. For a dollar of
US currency, the answer ought to be 292, but the 100th value is 293.
The 101th value is 292, which ought to be the correct answer. so..hmm.

see here:
https://www.maa.org/frank-morgans-math-chat-293-ways-to-make-change-for-a-dollar
--}