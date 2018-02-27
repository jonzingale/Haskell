module Listables where
-- should be able to write everybody as some minimal basis of functions.
-- take, drop, length, (!!), head, tail, (++), cons, reverse

class Eq m => Listable m where
  takeL :: Integer -> m -> m
  dropL :: Integer -> m -> m
  cons :: m -> m -> m
  (+++) :: m -> m -> m
  unit :: m

  (!!!) :: m -> Integer -> m
  lengthL :: m -> Integer
  reverseL :: m -> m
  headL :: m -> m
  tailL :: m -> m

  headL = takeL 1
  tailL = dropL 1
  (!!!) ls n = headL.dropL n $ ls

  lengthL ls | unit == ls = 0
             | otherwise = 1 + (lengthL.dropL 1) ls

  reverseL ns = ff ns unit
    where
      ff ns accum | ns == unit = accum
                  | otherwise = ff (tailL ns) (headL ns `cons` accum)

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