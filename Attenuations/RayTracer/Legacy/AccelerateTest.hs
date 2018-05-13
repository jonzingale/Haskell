
-- requires accelerate backend.
-- http://hackage.haskell.org/package/accelerate-llvm-native
module RayTracer.AccelerateTests where
import Prelude hiding (Num, zipWith)
import Data.Array.Accelerate

dotp :: Num a => Vector a -> Vector a -> Acc (Scalar a)
dotp xs ys =
  let
      xs' = use xs
      ys' = use ys
  in
  fold (+) 0 ( zipWith (*) xs' ys' )