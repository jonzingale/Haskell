{-# OPTIONS_GHC -Wno-missing-methods #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module Fourier where
import qualified Data.Vector.Unboxed as U
import Data.Complex

type C = Complex Float
num :: Complex Float
num = 2 :+ 3

sumNum = num + num
elemG = Zn 2 3
chi0 = Chi (\a -> eval a)
chi1 = Chi (\a -> eval $ a + (gen a) )

(<|>) :: Cyclic -> Cyclic -> C
(<|>) (Chi f) (Zn k m) = f (Zn k m)
test_inner1 = chi0 <|> elemG
test_inner2 = chi1 <|> (idG elemG)

data Cyclic = Zn Int Int | Chi ( Cyclic -> C ) -- k + mZ

instance Show Cyclic where
  show (Zn k m) = show (k `mod` m) ++ " + " ++ show m ++ "Z"
  show (Chi f) = "χ()"-- work this out

instance Num Cyclic where
  (+) (Zn k m) (Zn l n) = Zn (mod (k+l) m) m
  (+) (Chi f) (Chi g) = Chi (\a -> (f a) * (g a))
  negate (Zn k m) = Zn (-k) m
  negate (Chi f) = Chi $ f . negate

class Num a => Abelian a where
  chars :: a -> [a]
  char :: a -> a -> C
  dual :: a -> a
  elems :: a -> [a]
  ord :: a -> Int
  eval :: a -> C
  inv :: a -> a
  idG :: a -> a
  gen :: a -> a

  elems a = take (ord a) $ iterate (+ (gen a)) $ idG a
  char i = \j -> eval (i+j)
  inv a = negate a
  idG a = a - a

instance Abelian Cyclic where
  chars j = map (Chi . (\x a -> eval(x + a))) $ elems j

  eval (Zn x m) =
    let ratio = fromIntegral x / fromIntegral m in
    exp $ 2 * pi * ratio * (0 :+ (-1))

  gen (Zn a m) = Zn 1 m
  ord (Zn x m) = m

-- returns rootsUnity
test_chars = zipWith (<|>) (chars elemG) (elems elemG)




