{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fourier where
import qualified Data.Vector.Unboxed as U
import Data.Complex

type C = Complex Float
num :: Complex Float
num = 2 :+ 3

sumNum = num + num
elemG = Zn 2 3

data Cyclic = Zn Int Int | Chi ( Cyclic -> C ) -- k + mZ

instance Show Cyclic where
  show (Zn k m) = show (k `mod` m) ++ " + " ++ show m ++ "Z"
  show (Chi f) = "χ()"-- work this out

instance Num Cyclic where
  (+) (Zn k m) (Zn l n) = Zn (mod (k+l) m) m
  -- (+) (Chi f) (Chi g) = 
  (negate) (Zn k m) = Zn (mod (-k) m) m

class Num a => Abelian a where
  chars :: a -> [a -> C]
  char :: a -> a -> C
  elems :: a -> [a]
  ord :: a -> Int
  eval :: a -> C
  inv :: a -> a
  idG :: a -> a
  gen :: a -> a

  elems a = take (ord a) $ iterate (+ (gen a)) $ idG a
  chars a = map char (elems a)
  char i = \j -> eval (i+j)
  inv a = negate a
  idG a = a - a

instance Abelian Cyclic where
  eval (Zn x m) =
    let ratio = fromIntegral x / fromIntegral m in
    exp $ 2 * pi * ratio * (0 :+ (-1))
  gen (Zn a m) = Zn 1 m
  ord (Zn x m) = m

-- instance Show (Abelian a) where
--   show char a = ""

{-- TODO: can I extend char to Num?
instance Num Abelian a where
  (+) a b = elemG
--}







