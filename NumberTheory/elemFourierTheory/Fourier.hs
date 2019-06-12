{-# OPTIONS_GHC -Wno-missing-methods #-}

module Fourier where
import qualified Data.Vector.Unboxed as U
import Data.Complex


num :: Complex Float
num = 2 :+ 3

sumNum = num + num
congru = Z 2 3

data Cyclic x = Z x x deriving (Eq) -- k + mZ

instance (Show x, Integral x) => Show (Cyclic x) where
  show  (Z k m) = show (k `mod` m) ++ " + " ++ show m ++ "Z" 

instance (Num x, Integral x) => Num (Cyclic x) where
  (+) (Z k m) (Z l n) = Z (mod (k+l) m) m


{--
The homomorphism taking cyclic groups to roots of unity.
--}