module InfiniteLists where

integers :: VeryLargeArray Integer
integers = Array (map (* (-1)) [1..]) 0 [1..]

integerPlane :: VeryWideArray Integer
integerPlane = Wide integers integers

data VeryLargeArray a = Array [a] a [a]
data VeryWideArray a = Wide (VeryLargeArray a) (VeryLargeArray a)

instance Show a => Show (VeryLargeArray a) where
  show (Array xs y zs) = unwords [(show.reverse.take 10) xs, 
                                  "F"++ show [y],
                                  (show.take 10) zs] 

instance Show a => Show (VeryWideArray a) where
  show (Wide xs zs) = unlines.map show $ [xs, zs]

class Shiftable w where
  left, right :: w a -> w a
  focus :: w a -> a

instance Shiftable VeryLargeArray where
  left (Array (x:xs) y zs) = Array xs x (y:zs)
  right (Array xs y (z:zs)) = Array (y:xs) z zs
  focus (Array _ e _) = e

instance Shiftable VeryWideArray where
  left (Wide xs ys) = Wide (left xs) ys
  right (Wide xs ys) = Wide (right xs) ys
  focus (Wide (Array as b cs) (Array ds e fs)) = b -- this maybe cannot be.

class Shiftable w => BiShiftable w where
  down, up :: w a -> w a

instance BiShiftable VeryWideArray where
  down (Wide xs ys) = Wide xs (left ys)
  up (Wide xs ys) = Wide xs (right ys)

class Shiftable a => RightAbelian a where -- R x A -> A
  plus :: Integer -> a x -> a x

  plus n vlArray | n > 0 = plus (n-1) $ right vlArray
                 | n < 0 = plus (n+1) $ left vlArray
                 | otherwise = vlArray

instance RightAbelian VeryLargeArray where