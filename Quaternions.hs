module Quaternions where

data Q a = Q a a a a deriving (Eq, Show)

-- a + bi + cj + dk
-- i^2 = j^2 = k^2 = ijk = −1

-- instance (Eq a) => Eq (Mod a) where
--   Q a b c d == Q e f g h | a == c && b == d = True
--                          | otherwise = False

-- perhaps another instance Eq a for the 2 to 1 projection.

-- ‘*’, ‘abs’, ‘signum’, ‘fromInteger’, and (either ‘negate’ or ‘-’)
instance (Num a, Integral a) => Num (Q a) where
  Q a b c d + Q e f g h = Q (a+e) (b+f) (c+g) (d+h)
  Q a b c d * Q e f g h =
      let u = a*e-b*f-c*g-d*h in
      let i = a*f+b*e+c*h-d*g in
      let j = a*g-b*h+c*e+d*f in
      let k = a*h+b*g-c*f+d*e in
      Q u i j k
  fromInteger n = Q (fromInteger n) 0 0 0
  -- Q a b c d + Q e f g h = Q (a+e) (b+f) (c+g) (d+h) 