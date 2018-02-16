module ComplexVector where
import Complex

cv1 = V3 (C 1 (-1)) (C 2 3) (C 5 0)

data Vector x = S x | V3 x x x | Bad deriving (Show, Eq)

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
  conj vect = fmap conj vect
  (<|>) a b = a * b

instance (Floating v, Comp v, Num v) => Num (Vector v)  where
  (+) (S a) (S b) = S $ a + b
  (+) (V3 _ _ _) (S _) = Bad
  (+) (S _) (V3 _ _ _) = Bad
  (+) (V3 a b c) (V3 d e f) = V3 (a+d) (b+e) (c+f)

  (*) (S a) (S b) = S $ a * b
  (*) (S a) (V3 d e f) = V3 (a*d) (a*e) (a*f)
  (*) (V3 d e f) (S a) = V3 (a*d) (a*e) (a*f)
  (*) v w = (*) <$> v <*> (fmap conj w)

  fromInteger x = S $ fromInteger x
  abs v = S $ sqrt $ eval $ v <|> v
  signum = id -- not sure what this means.
  negate v = fmap (* (-1)) v
