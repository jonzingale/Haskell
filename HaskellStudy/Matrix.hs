module MatrixFunctorial where
import VectorNonFunc

data ThreeMatrix = M { p1 :: ThreeVect,
                       p2 :: ThreeVect,
                       p3 :: ThreeVect } deriving (Eq)

instance Show ThreeMatrix where
  show (M a b c) = unlines.map show $ [a, b, c]

idM = M (V 1 0 0) (V 0 1 0) (V 0 0 1)
ms = M vs ws vs
ns = M ws ws vs

-- because I don't have Functor ThreeMarix.
mmap f (M a b c) = M (f a) (f b) (f c)

evalM :: ThreeMatrix -> ThreeVect -> ThreeVect
evalM (M a b c) v = V (eval (a * v)) (eval (b * v)) (eval (c * v))

tr :: ThreeMatrix -> ThreeMatrix
tr (M (V a b c) (V d e f) (V g h i)) = M (V a d g) (V b e h) (V c f i)

trace :: ThreeMatrix -> ThreeVect
trace (M (V a _ _) (V _ b _) (V _ _ c)) = V a b c

instance Num ThreeMatrix where
  fromInteger t = let tt = fromInteger t in
    M (V tt 0 0) (V 0 tt 0) (V 0 0 tt) -- t * idM

  abs m = mmap (vmap abs) $ m
  signum m = mmap (vmap sqrt) $ m * m

  (+) (M a b c) (M x y z) = M (a + x) (b + y) (c + z)
  (-) (M a b c) (M x y z) = M (a - x) (b - y) (c - z)

  (*) vs ws = let [a, b, c, d, e, f, g, h, i] = evalfs vs ws in
    M (V a b c) (V d e f) (V g h i)
      where
        evalfs = \ vs ws -> [ evalf p q vs ws | p <- prs, q <- prs ]
        evalf pp qq = \ v w -> eval (pp v * ((qq.tr) w))
        prs = [p1, p2, p3]
