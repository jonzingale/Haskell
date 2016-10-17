-- Cohomology and Arithmetic
module Cohomology_Arithmetic where
{-# LANGUAGE ExistentialQuantification #-}

import Data.Monoid

class Monoid a => Group a where
  inv :: a -> a

data Z_10 = Z Integer -- deriving (Show)
data Z_100 = ZZ Z_10 Z_10 -- deriving (Show)

type Tens = Z_10
type Ones = Z_10

instance Num Z_10 where
  (Z a) + (Z b) = Z $ (a + b) `mod` 10
  fromInteger x = Z $ (fromInteger x) `mod` 10
  (Z a) * (Z b) = Z $ (a * b) `mod` 10
  negate (Z a) = Z $ (- a) `mod` 10
  signum _ = 1
  abs a = a

instance Num Z_100 where
  (ZZ a b) + (ZZ c d) = ZZ (a + c + z2 b d) (b + d) --cocycle
  (ZZ a b) * c = mconcat.replicate (toInt a * 10 + toInt b) $ c
  negate (ZZ a b) = ZZ (inv a - 1) (inv b)
  fromInteger x = ZZ (Z 0) (Z x)
  signum _ = 1
  abs a = a

instance Show Z_10 where
  show (Z a) = show.toInteger $ a `mod` 10

instance Show Z_100 where
  show (ZZ (Z a) (Z b)) = show.toInteger $
    (a * 10 `mod` 100) + (b `mod` 10) 

instance Eq Z_10 where
  (Z a) == (Z b) = a `mod` 10 == b `mod` 10

instance Eq Z_100 where
  (ZZ a b) == (ZZ c d) = a == c && c == d 

instance Monoid Z_10 where
  mconcat = foldr mappend mempty
  mappend a b = a + b
  mempty = Z 0

instance Monoid Z_100 where
  mconcat = foldr mappend mempty
  mempty = ZZ (Z 0) (Z 0)
  mappend a b = a + b

instance Group Z_10 where
  inv (Z a) = Z (-1 * a)

instance Group Z_100 where
  inv a = negate a

toInt :: Z_10 -> Int
toInt (Z x) = fromIntegral x

z :: Ones -> Ones -> Tens
z (Z a) (Z b) | a + b > 9 = Z 1
              | otherwise = Z 0

z2 a b = (* 1) . z a $ b