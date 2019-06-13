{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module FourierFamily where
import qualified Data.Vector.Unboxed as U
import Data.Complex

newtype Cx = Complex Double
type Zn = Cyclic Int

num :: Complex Double
num = 2 :+ 3

sumNum = num + num

congru :: Cyclic Int
congru = Z 2 3

data Cyclic x = Z x x deriving (Eq) -- k + mZ

instance (Show x, Integral x) => Show (Cyclic x) where
  show  (Z k m) = show (k `mod` m) ++ " + " ++ show m ++ "Z" 

instance (Num x, Integral x) => Num (Cyclic x) where
  (+) (Z k m) (Z l n) = Z (mod (k+l) m) m


class (Show cyclic, Show complex) => Character cyclic complex where
  eval :: cyclic -> complex

instance (Integral x, Show x, RealFloat t, Show t) =>
  Character (Cyclic x) (Complex t) where
  eval (Z x n) = 
    let ratio = fromIntegral x / fromIntegral n in
    exp $ 2 * pi * ratio * (0 :+ (-1))

test = eval congru::(Complex Float)