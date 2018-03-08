{--
Exterior Algebras and Alternating Tensors.

a p-form on a manifold X is a function which
takes a point x<-X and returns an alternating
tensor on the tangent Space TX.

(X) :: Tensor(p) -> Tensor(q) -> Tensor(p+q)
(X) [[a10..a1k],[...],[ak0...akk]] B = [[a10B..a1kB],[...],[ak0B..akkB]]

(^) :: LambTp -> LambTq -> LambT(p+q)
(^) = Alt.(X)


Properties of Alt as Monad?
1) T <- TensorK(V) => Alt(T) <- LambK(V) # inclusion 
2) w <- LambK(V) => Alt(w) = w # idempotent
3) T <- TensorK(V) => Alt.Alt(T) = Alt(T) # also idempotent?
--}

{-# OPTIONS_GHC -Wno-missing-methods #-}

module AlternatingTensors where
import Data.List -- for permutations

data Algebra = Tensr | Alt Algebra deriving (Eq)

instance Show Algebra where
  show Tensr = "T (v1...vk)"
  show (Alt t) = "Alt(T (v1...vk))"

newtype Tensor = T [Double] deriving (Show, Eq)
-- instance Show Tensor where (something with dxs?)

tensor = T [4.0,3.0,1.0,4.0]

instance Num a => Num [a] where
  (+) a b = zipWith (+) a b
  (*) a b = zipWith (*) a b
  fromInteger a = fromInteger a : fromInteger a

class Alternating a where
  alt :: a -> a
  (><) :: a -> a -> a -- to be implemented.

instance Alternating Tensor where
  alt (T ts) = T $ (fromScalar.invFact) ts * (sumT.f) ts
    where -- 1/k! Σ sgn(σ) * T(vσ1...vσk)
      f as = [map (* (sgn t)) t | t<-permutations as]
      invFact n = 1 / foldr (*) 1 [1..len n]
      fromScalar n = n : fromScalar n
      len = fromIntegral.length
      sumT = foldr (+) 0

      swap a b = map $
          \x-> if x==a then b
               else if x==b then a
               else x

      sgn [] = 1
      sgn (x:xs) = case x of
          1 -> sgn $ map (subtract 1) xs
          _ -> (* (-1)) . sgn $ swap 1 x (x:xs)

