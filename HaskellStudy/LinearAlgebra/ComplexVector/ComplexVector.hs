{-# OPTIONS_GHC -Wno-missing-methods #-} -- because of signum.

module ComplexVector where
import Complex
-- import Data.Complex

cv1 = V3 (C 1 (-1)) (C 2 3) (C 5 0)
dd = V3 (C 0 0) (C 1 (-1)) (C 0 0)
cc = V3 (C 1 0) (C 0 0) (C 0 0)

ee = V3 (C 1 1) (C 2 (-1)) (C 0 0)
ff = V3 (C 3 (-2)) (C 1 1) (C 0 0)

data Vector x = S x | V3 x x x | Bad deriving Eq

instance (Show a) => Show (Vector a) where
  show (V3 a b c) = show [a, b, c]
  show (S a) = show a

eval :: Num x => Vector x -> x
eval (V3 a b c) = a + b + c
eval (S x) = x

instance Functor Vector where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)
  fmap f (S x) = S (f x)

instance Applicative Vector where
  (V3 f g h) <*> (V3 x y z) = V3 (f x) (g y) (h z)
  pure t = S t

instance (Floating c, Num c, Comp c) => Comp (Vector c) where
  -- note that for complex vectors cross product is sesquilinear
  (%) (V3 a b c) (V3 x y z) = fmap conj $ V3 (b*z-y*c) (c*x-a*z) (a*y-b*x)
  -- (%) (V3 a b c) (V3 x y z) = fmap conj $ V3 (b*z-y*c) (c*x-a*z) (a*y-b*x) -- W x U* ?
  conj vect = fmap conj vect
  (<|>) a b = a * b  -- Hermitian

instance (Floating v, Comp v, Num v) => Num (Vector v)  where
  (+) (S a) (S b) = S $ a + b
  (+) (V3 _ _ _) (S _) = Bad
  (+) (S _) (V3 _ _ _) = Bad
  (+) (V3 a b c) (V3 d e f) = V3 (a+d) (b+e) (c+f)

  (*) (S a) (S b) = S $ a * b
  (*) (S a) (V3 d e f) = V3 (a*d) (a*e) (a*f)
  (*) (V3 d e f) (S a) = V3 (a*d) (a*e) (a*f)
  (*) v w = (*) <$> w <*> (fmap conj v) -- Hermitian

  fromInteger x = S $ fromInteger x
  abs v = S $ sqrt $ eval $ v <|> v
  negate v = fmap (* (-1)) v
  signum = id

conjTest :: Vector Complex -> Vector Complex -> (Complex, Bool)
conjTest (V3 a b c) (V3 x y z) = -- <(z,w)|(x,y)>*
  let f = \a b c d -> (conj a * c) + (conj a * d) + (conj b * c) + (conj b * d) in
  (f a b x y , (f a b x y) == (conj  (f x y a b)))


