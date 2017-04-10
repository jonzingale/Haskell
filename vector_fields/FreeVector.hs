module FreeVector where
import Modulo
{--
Part of the idea here is to understand
the counit for a free vector construction
over a finite field.

counit :: FG K -> K
1a + 0b + 0c -> a
0a + 1b + 0c -> b
0a + 0b + 1c -> c

and the rest follows some how.

perhaps i could construct a coAlgebra
with tensor_x, and counit as above.

This may not be the clearest way to approach
the problem of building up a linear algebra
from an underlying finite field. I am tempted
to some how recast FiniteField as a class
instead of relying on Mod, an extension of the
Num class, defined in the Modulo module.
The advantage would be that I could in theory
extend Vectors to do FiniteField like computations.
Then again, maybe that thinking isn't correct either.
--}

--------Vector Logic
data V3 k x = V { field :: k, p1 :: x, p2 :: x, p3 :: x } deriving (Show, Eq)

v1 :: V3 Integer Integer
v1 = V 7 3 2 4

--------Field Logic
type FiniteField k = Mod k

k1, k2 :: FiniteField Integer
k1 = Mod 7 4
k2 = Mod 7 2

(//) :: Integral k => FiniteField k -> FiniteField k -> FiniteField k
(//) a b = a * (inv_k b)

inv_k :: Integral k => FiniteField k -> FiniteField k
inv_k (Mod n k) | prime n = inv (Mod n k)
                | otherwise = BAD

prime :: Integral k => k -> Bool
prime n = let root = (\x -> floor $ (fromIntegral x) ** 0.5) in
  foldr (&&) True [False | k <- [2..root n], n `mod` k == 0]
