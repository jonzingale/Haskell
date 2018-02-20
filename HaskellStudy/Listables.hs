module Listables where

-- should be able to write everybody with just head tail and cons.
-- take, drop, length, (!!), head, tail, (++), cons

class Listable m where
  takeL :: Integer -> m -> m
  dropL :: Integer -> m -> m
  cons :: m -> m -> m
  unit :: m

lengthL :: (Eq a, Listable a) => a -> Integer
lengthL ls | unit == ls = 0
           | otherwise = 1 + (lengthL.dropL 1) ls

headL, tailL :: Listable a => a -> a
headL = takeL 1
tailL = dropL 1

(+++) :: (Eq a, Listable a) => a -> a -> a -- error in ordering.
(+++) ns ms | ms == unit = ns
            | otherwise = (+++) ((headL ms) `cons` ns) (tailL ms)

instance Listable Integer where
  dropL n zs = div zs (10^n)
  takeL n zs = mod zs (10^n)
  cons n ns = ns * 10 + n
  unit = 0
