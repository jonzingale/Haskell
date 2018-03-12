module Solution15 where

{-- Lattice paths:
Starting in the top left corner of a 2×2 grid,
and only being able to move to the right and down,
there are exactly 6 routes to the bottom right corner.
--}

combs :: Integer -> Integer -> Integer
combs 0 0 = 1
combs 0 _ = 0
combs n k = combs (n-1) (k-1) + combs (n-1) k

-- The above combinations algorithm is too slow, so instead:
-- http://mattwetmore.me/posts/n-choose-k-the-haskell-way.html

instance Num a => Num [a] where
  fromInteger n = [fromInteger n]
  (x:xs) + (y:ys) = (x + y) : (xs + ys)
  xs + [] = xs
  [] + ys = ys
  (x:xs) * (y:ys) = (x*y) : ([x] * ys + xs * (y:ys))
  _ * _ = []

choose :: Int -> Int -> Int
choose n k = ([1,1]^n) !! k

{-- 
3x3:
rrrddd
rrdrdd
6 choose 3

so . . . 40 choose 20
40 steps in which half are right and half are down.
--}