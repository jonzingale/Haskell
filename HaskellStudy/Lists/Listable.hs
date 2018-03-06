module Listable where
-- should be able to write everybody as some minimal basis of functions.
-- take, drop, length, (!!), head, tail, (++), cons, reverse

class Eq m => Listable m where
  takeL, dropL :: Integer -> m -> m
  filterL :: (m -> Bool) -> m -> m
  cons :: m -> m -> m
  (+++) :: m -> m -> m
  unit :: m

  (!!!) :: m -> Integer -> m
  lengthL :: m -> Integer
  headL, tailL :: m -> m
  reverseL :: m -> m

  (!!!) ls n = headL.dropL n $ ls
  headL = takeL 1
  tailL = dropL 1

  lengthL ls | unit == ls = 0
             | otherwise = 1 + (lengthL.dropL 1) ls

  reverseL ns = ff ns unit
    where
      ff ns accum | ns == unit = accum
                  | otherwise = ff (tailL ns) $ headL ns `cons` accum

  filterL b ns = f b ns unit
    where
      f b js accum | js == unit  = accum
                   | (b.headL) js = f b (tailL js) $ headL js +++ accum
                   | otherwise   = f b (tailL js) accum

instance Listable Integer where
  (+++) ns ms = ms + ns * 10 ^ lengthL ms
  dropL n zs = div zs $ 10^n
  takeL n zs = mod zs $ 10^n
  cons n ns = ns * 10 + n
  unit = 0

instance Eq a => Listable [a] where
  dropL = drop.fromIntegral
  takeL = take.fromIntegral
  cons [n] ns = n : ns -- The types are funny here
  (+++) = (++)
  unit = []