module LinearAlgebra where
{--
  Given any pair, derive the other.

  given d & f: pullback
  given f & d: invertible d (section)
  give f & g: linear map (coalgebra?)

       φ
    A ---> B
     \    /
    f \  / g
       R
--}

data Vector x = S x | V2 x x | V3 x x x deriving Show

scalar = S 5
v2 = V2 3 4
v3 = V3 1 2 3
f2 = V2 (\i j -> i + 2*j) (\i j -> 3*i + j)

instance Functor Vector where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)
  fmap f (V2 x y) = V2 (f x) (f y)
  fmap f (S x) = S $ f x

instance Applicative Vector where
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  (V3 f g h) <*> (V3 x y z) = V3 (f x) (g y) $ h z
  (V2 f g) <*> (V2 x y) = V2 (f x) $ g y
  (S f) <*> (S x) = S $ f x

instance Num x => Num (Vector x)  where
  fromInteger x = S $ fromInteger x
  (+) (V3 a b c) (V3 s t u) = V3 (a + s) (b + t) (c + u)
  (+) (V2 s t) (V2 u v) = V2 (s + u) (t + v)
  (+) (S s) (S u) = S (s + u)

  (*) (S r) vect = (<$>) (\j -> j*r) vect -- transpose (S r) ?

  abs v = v <|> v -- what about V* <|> V
  -- negate v = (S (-1)) * v
  -- signum vect =  

class Num v => Linear v where
  (<|>) :: v -> v -> v
  eval :: v -> v
  tr :: v -> v

-- tr :: Num a => Vector a -> Vector (a -> a -> a)
-- tr (V2 x y) = S (\i j -> x*i + y*j)

instance Num v => Linear (Vector v) where
  (<|>) (V3 a b c) (V3 x y z) = V3 (a * x) (b * y) (c * z)
  (<|>) (V2 a b) (V2 x y) = V2 (a * x) (b * y)
  (<|>) (S a) (S x) = S $ a * x

  eval (V3 x y z) = S (x + y + z)
  eval (V2 x y) = S (x + y)
  eval (S a) = S a

  -- tr (V2 x y) = S (\x -> x) --S (\i j -> x*i + y*j)

