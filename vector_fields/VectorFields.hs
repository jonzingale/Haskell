module VectorFields where
{--
Explore the forms of various
operators on vector fields.

Todo: extend Numeric Vectors to linear functions.
      signum based on cross product?
--}

data Vector x = S x | V_2 x x | V_3 x x x
  deriving Show

scalar = S 5
v2 = V_2 3 4
v3 = V_3 1 2 3

instance (Num x) => Num (Vector x)  where
  fromInteger x = S $ fromInteger x
  (+) (V_3 a b c) (V_3 s t u) = V_3 (a + s) (b + t) (c + u)
  (+) (V_2 s t) (V_2 u v) = V_2 (s + u) (t + v)
  (+) (S s) (S u) = S (s + u)

  (*) (S r) (V_3 s t u) = V_3 (r * s) (r * t) (r * u)
  (*) (S r) (V_2 s t) = V_2 (r * s) (r * t)
  (*) (S r) (S x) = S (r * x)

  abs (V_2 s t) = S $ (s^2 + t^2) -- needs root
  abs (S x) = S x

  negate vector = (S (-1)) * vector
  -- signum (V_3 x y z) = 
  -- signum (V_2 x y) = 
  signum (S x) = S $ signum x

class (Num v, Show v) => Linear v where
  dot :: v -> v -> v
  eval :: v -> v

instance (Num v, Show v) => Linear (Vector v) where
  dot (V_3 a b c) (V_3 x y z) = V_3 (a * x) (b * y) (c * z)
  dot (V_2 a b) (V_2 x y) = V_2 (a * x) (b * y)
  dot (S a) (S x) = S $ a * x

  eval (V_3 x y z) = S (x + y + z)
  eval (V_2 x y) = S (x + y)
  eval (S a) = S a


-- det :: Matrix a -> Float