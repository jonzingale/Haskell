{-# OPTIONS_GHC -fno-warn-tabs #-}

module Modulo where

data Mod m = Mod { modulus :: m, value :: m } | BAD deriving (Show)

incl :: (Integral a) => a -> a -> Mod a
incl n a = Mod n a

rep :: (Integral a) => Mod a -> a
rep (Mod n a) = mod a n

instance (Eq a) => Eq (Mod a) where
	Mod a b == Mod c d | a == c && b == d = True
										 | otherwise = False

instance (Num a, Integral a) => Num (Mod a) where
	Mod a b + Mod c d  | a == c = Mod a $ (b + d) `mod` a
										 | otherwise = BAD

	Mod a b * Mod c d | a == c = Mod a $ (b * d) `mod` a
										| otherwise = BAD

	negate (Mod a b) = Mod a $ (negate b) `mod` a
	fromInteger a = incl (fromIntegral a) 0
	abs (Mod a b) = Mod a $ b `mod` a
	signum (Mod a b) = 1

this = Mod 7 5
that = Mod 3 11