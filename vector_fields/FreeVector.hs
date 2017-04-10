module FreeVector where
import Modulo
-- there is likely a nice way to import.

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
--}

-- Ideally would this be a finite field.
-- The underlying field is Z3
-- type Field = Integer

-- data V3 k x = Nil | V k x x x deriving (Show, Eq)

type FiniteField k = Mod k

k1, k2 :: FiniteField Integer
k1 = (Mod 7 4)
k2 = (Mod 7 2)

(//) :: Integral k => FiniteField k -> FiniteField k -> FiniteField k
(//) a b = a * (inv_k b)

inv_k :: Integral k => FiniteField k -> FiniteField k
inv_k (Mod n k) | prime n = inv (Mod n k)
                | otherwise = BAD

prime :: Integral k => k -> Bool
prime n = let root = (\x -> floor $ (fromIntegral x)**0.5) in
  foldr (&&) True [False | k <- [2..root n], n `mod` k == 0]
