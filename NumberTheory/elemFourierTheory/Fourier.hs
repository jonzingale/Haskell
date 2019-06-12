{-# OPTIONS_GHC -Wno-missing-methods #-}

module Fourier where
import qualified Data.Vector.Unboxed as U
import Data.Complex


num :: Complex Float
num = 2 :+ 3

sumNum = num + num
congru = Z 2 3

data Cyclic x = Z x x deriving (Eq) -- k + mZ

-- todo: write Eq instance

instance (Show x, Integral x) => Show (Cyclic x) where
  show  (Z k m) = show (k `mod` m) ++ " + " ++ show m ++ "Z" 

instance (Num x, Integral x) => Num (Cyclic x) where
  (+) (Z k m) (Z l n) = Z (mod (k+l) m) m


{--
The homomorphism taking cyclic groups to roots of unity.
It may be good to verify that this is in fact a homomorphism.
chi(0) = 1, chi(x)*chi(y) = chi(x+y)
--}

chi :: Integral x => Cyclic x -> Complex Float
chi (Z x m) =
  let ratio = fromIntegral x / fromIntegral m in
  exp $ 2 * pi * ratio * (0 :+ (-1))

{--
Towards an L2(G) class, the n-dimensional
vector space of complex valued functions on G

Note: This class seems possible to write exactly
because G dual is finite.
--}

del :: Integral x => Cyclic x -> Cyclic x -> Complex Float
del a x | a == x = 1
        | otherwise = 0

