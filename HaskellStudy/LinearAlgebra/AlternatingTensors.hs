{--
Exterior Algebras and Alternating Tensors.

a p-form on a manifold X is a function which
takes a point x<-X and returns an alternating
tensor on the tangent Space TX.

(X) :: Tensor(p) -> Tensor(q) -> Tensor(p+q)
(X) [[a10..a1k],[...],[ak0...akk]] B = [[a10B..a1kB],[...],[ak0B..akkB]]

(^) :: LambTp -> LambTq -> LambT(p+q)
(^) = Alt.(X)

--}

module AlternatingTensors where

