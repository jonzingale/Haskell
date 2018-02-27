module DataVector where
import qualified Data.VectorSpace as VS
import Data.Vector

ten, sen :: Vector Integer
ten = generate 10 (fromIntegral.(+1))
sen = fromList [1..10]

instance Num a => Num (Vector a) where
  (+) v w = fromList $ Prelude.zipWith (+) (toList v) (toList w)
  (*) v w = fromList $ Prelude.zipWith (*) (toList v) (toList w)
  (-) v w = fromList $ Prelude.zipWith (-) (toList v) (toList w)