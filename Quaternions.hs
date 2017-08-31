module Quaternions where

data Q a = Q a a a a deriving (Eq, Show)

conj :: Num a => Q a -> Q a
conj (Q a b c d) = Q a (-b) (-c) (-d)

-- a + bi + cj + dk
-- i^2 = j^2 = k^2 = ijk = −1

-- instance (Eq a) => Eq (Mod a) where
--   Q a b c d == Q e f g h | a == c && b == d = True
--                          | otherwise = False

-- perhaps another instance Eq a for the 2 to 1 projection.

-- ‘abs’, ‘signum’, and (either ‘negate’ or ‘-’)
instance (Num a, Integral a) => Num (Q a) where
  fromInteger n = Q (fromInteger n) 0 0 0
  Q a b c d + Q e f g h = Q (a+e) (b+f) (c+g) (d+h)
  Q a b c d * Q e f g h =
      let u = a*e-b*f-c*g-d*h in
      let i = a*f+b*e+c*h-d*g in
      let j = a*g-b*h+c*e+d*f in
      let k = a*h+b*g-c*f+d*e in
      Q u i j k

-- as this will not be an Integer value.
abs (Q a b c d) = Q ((a*a + b*b + c*c + d*d)**0.5) 0 0 0
