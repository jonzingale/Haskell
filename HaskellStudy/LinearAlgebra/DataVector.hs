{-# OPTIONS_GHC -Wno-missing-methods #-} -- because of signum, fromInteger, *

module DataVector where
import qualified Data.VectorSpace as VS
import Data.Vector
import Data.Complex

ten, sen :: Vector Integer
ten = generate 10 (fromIntegral.(+1))
sen = fromList [1..10]

binOp :: (a -> a -> a) -> Vector a -> Vector a -> Vector a
binOp bin a b = fromList $ Prelude.zipWith bin (toList a) (toList b)

instance Num a => Num (Vector a) where
  (+) v w = binOp (+) v w
  -- (*) v w = binOp (*) v w
  (-) v w = binOp (-) v w
  abs v = v