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

-- Cyclic
-- Should Cyclic be a class? ord, data, etc ...
data Cyclic x = Z x x deriving Eq -- k + mZ

instance (Show x, Integral x) => Show (Cyclic x) where
  show  (Z k m) = show (k `mod` m) ++ " + " ++ show m ++ "Z" 

instance (Num x, Integral x) => Num (Cyclic x) where
  (+) (Z k m) (Z l n) = Z (mod (k+l) m) m
-----

-- Cyclic 2
class (Num a) => Abelian a where
  char :: a -> Complex Double
  sum :: (a -> Complex Double) -> (a -> Complex Double) -> Complex Double
  -- char 
-----


-- Characters
class (Num abGroup) => Character abGroup where
  eval :: abGroup -> Complex Double

instance Integral x => Character (Cyclic x) where
  eval (Z x n) =
    let ratio = fromIntegral x / fromIntegral n in
    exp $ 2 * pi * ratio * (0 :+ (-1))
-----

-- instance Num (Character) where
  -- (+) x1 x2 = eval x1 * eval x2
















