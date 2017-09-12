module Quaternions where

data Q a = Q a a a a deriving (Eq, Show)

-- a + bi + cj + dk
-- i^2 = j^2 = k^2 = ijk = −1

qq = Q 1 2 3 4 
uu = Q 5 2 3 1

conj :: Num a => Q a -> Q a
conj (Q a b c d) = Q a (-b) (-c) (-d)

-- ‘abs’, ‘signum’, and (either ‘negate’ or ‘-’)
instance (Num a, Integral a) => Num (Q a) where
  Q a b c d + Q e f g h = Q (a+e) (b+f) (c+g) (d+h)
  fromInteger n = Q (fromInteger n) 0 0 0
  negate qq = qq * Q (-1) 0 0 0
  Q a b c d * Q e f g h =
      let u = a*e-b*f-c*g-d*h in
      let i = a*f+b*e+c*h-d*g in
      let j = a*g-b*h+c*e+d*f in
      let k = a*h+b*g-c*f+d*e in
      Q u i j k

-- as this will not be an Integer value.
abs (Q a b c d) = Q ((a*a + b*b + c*c + d*d)**0.5) 0 0 0
