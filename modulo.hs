module Modulo where
-- :set -XFlexibleContexts

data I_I m = I_I { modulus :: m, value :: m } | BAD deriving (Show)

incl :: (Integral a) => a -> a -> I_I a
incl n a = I_I n a

rep :: (Integral a) => I_I a -> a
rep (I_I n a) = mod a n

instance (Eq a) => Eq (I_I a) where
	I_I a b == I_I c d | a == c && b == d = True
										 | otherwise = False

instance (Num a, Integral a) => Num (I_I a) where
	I_I a b + I_I c d  | a == c = I_I a $ (b + d) `mod` a
										 | otherwise = BAD

this = I_I 7 5
that = I_I 3 11