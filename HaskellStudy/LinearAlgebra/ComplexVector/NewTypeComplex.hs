module NewTypeComplex where
import Complex

data ThreeVector a = V3 a a a deriving (Eq, Show)

newtype Vector a = Vector { getVect :: ThreeVector a }

instance Functor Vector where
  fmap f (Vector (V3 x y z)) = Vector $ V3 (f x) (f y) (f z)

-- abs vect = let (CV a b c) = vect <|> vect in
--   CV (sqrt a) (sqrt b) (sqrt c)


sqrtV :: Floating a => ThreeVector a -> ThreeVector a
sqrtV = \v -> getVect.fmap sqrt $ Vector v