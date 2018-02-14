module MatrixFunctorial where
import VectorFunctorial

data ThreeMatrix a = M { p1 :: (ThreeVect a), 
                         p2 :: (ThreeVect a),
                         p3 :: (ThreeVect a) } deriving (Show, Eq)

-- write a pretty and explicit show method.

instance Functor ThreeMatrix where
  fmap f (M a b c) = M (f <$> a) (f <$> b) (f <$> c)

ms = M vs ws vs
ns = M ws ws vs

class Matrix w where
  tr :: w -> w -- transpose

instance Num v => Matrix (ThreeMatrix v) where
  tr (M (V a b c) (V d e f) (V g h i)) = M (V a d g) (V b e h) (V c f i)

instance (Floating v, Num v) => Num (ThreeMatrix v) where
  -- fromInteger x = fromInteger <$> V x x x -- fascilitates scalar multiplication
  (+) (M a b c) (M x y z) = M (a+x) (b+y) (c+z)
  -- (*) vs ws = M (f vs ws) (g vs ws) (h vs ws)
  --   where
  --     f mm nn = V ((p1 mm) * ((p1.tr) nn)) ((p1 mm) * ((p2.tr) nn)) ((p1 mm) * ((p3.tr) nn))
  --     g mm nn = V ((p2 mm) * ((p1.tr) nn)) ((p2 mm) * ((p2.tr) nn)) ((p2 mm) * ((p3.tr) nn))
  --     h mm nn = V ((p3 mm) * ((p1.tr) nn)) ((p3 mm) * ((p2.tr) nn)) ((p3 mm) * ((p3.tr) nn))

  -- (-) (V a b c) (V x y z) = V (a-x) (b-y) (c-z)
  -- abs vect = sqrt <$> vect * vect
  -- signum vect = signum <$> vect

mult :: Floating a => ThreeMatrix a -> ThreeMatrix a -> ThreeMatrix (ThreeVect a)
mult vs ws = M (f vs ws) (g vs ws) (h vs ws)
  where
    f mm nn = V ((p1 mm) * ((p1.tr) nn)) ((p1 mm) * ((p2.tr) nn)) ((p1 mm) * ((p3.tr) nn))
    g mm nn = V ((p2 mm) * ((p1.tr) nn)) ((p2 mm) * ((p2.tr) nn)) ((p2 mm) * ((p3.tr) nn))
    h mm nn = V ((p3 mm) * ((p1.tr) nn)) ((p3 mm) * ((p2.tr) nn)) ((p3 mm) * ((p3.tr) nn))
