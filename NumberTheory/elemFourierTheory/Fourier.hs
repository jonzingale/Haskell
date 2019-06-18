{-# OPTIONS_GHC -Wno-missing-methods #-}

module Fourier where
import Data.Complex

data Cyclic = Zn Int Int | Chi ( Cyclic -> C )
type C = Complex Float

instance Show Cyclic where
  show (Zn k m) = show (k `mod` m) ++ " + " ++ show m ++ "Z"
  show (Chi f) = "χ()"

instance Eq Cyclic where
  (==) (Zn k m) (Zn l n) = k == l && m == n

instance Num Cyclic where
  (+) (Zn k m) (Zn l n) = Zn (mod (k+l) m) m
  (+) (Chi f) (Chi g) = Chi (\a -> f a * g a)
  (*) (Zn k m) (Zn l n) = Zn (mod (k*l) m) m

  negate (Zn k m) = Zn (-k) m
  negate (Chi f) = Chi $ f . negate

class Num a => Abelian a where
  (<||>) :: a -> a -> C
  (<|>) :: a -> a -> a -> C
  (<+>) :: a -> a -> a -- needs instance.
  chars :: a -> [a]
  elems :: a -> [a]
  ord :: a -> Int
  eval :: a -> C
  dual :: a -> a
  inv :: a -> a
  idG :: a -> a
  gen :: a -> a

  elems a = take (ord a) $ iterate (+ (gen a)) $ idG a
  inv a = negate a
  idG a = a - a

approx :: C -> C
approx n | abs(imagPart n) < eball && abs(realPart n) < eball = 0 :+ 0
         | abs(imagPart n) > eball && abs(realPart n) < eball = 0 :+ imagPart n
         | abs(imagPart n) < eball = realPart n :+ 0
         | otherwise = n
  where eball = 0.0001

instance Abelian Cyclic where
  (<||>) (Chi f) (Zn k m) = approx $ f (Zn k m)
  (<|>) (Chi f) (Chi h) g = (approx.sum) [(f.inv) gi * h gi | gi <- elems g]
  (<+>) (Zn a m) (Zn b n) = undefined -- Direct Sum G1 <+> G2 ??
  chars a = [Chi (\g -> eval $ g * i) | i <- elems a]
  dual (Zn a m) = Chi $ \j -> eval $ j * (Zn a m)
  dual (Chi f) = undefined -- needs a choice of basis?
  gen (Zn a m) = Zn 1 m
  ord (Zn x m) = m

  eval (Zn x m) =
    let ratio = fromIntegral x / fromIntegral m in
    approx $ exp $ 2 * pi * (0 :+ 1) * ratio
