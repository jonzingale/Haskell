{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SampleVector where
import qualified Data.Vector.Unboxed as U
import Data.Vector.Algorithms.Merge (sort)
import Data.Int (Int32)
import System.Random
import Data.WAVE

type Sample = Int32
type KeyedSamples = U.Vector (Int, Int32)

keySamples :: [Int32] -> KeyedSamples
keySamples xs = U.fromList $ zip [0..] xs

unkey :: KeyedSamples -> [Int32]
unkey ks = snd.unzip $ U.toList $ ks

-- update
uArray :: U.Unbox a => Int -> a -> U.Vector a -> U.Vector a
uArray t v a = (U.//) a [(t, v)]

-- bulk update
bArray :: U.Unbox a => [(Int, a)] -> U.Vector a -> U.Vector a
bArray tvs a = (U.//) a tvs

-- query
qArray :: U.Unbox a => Int -> U.Vector a -> a
qArray t a = (U.!) a t

wavToVect :: String -> IO KeyedSamples
wavToVect file = do
  wav <- getWAVEFile file
  let stereo = waveSamples wav
  let mono = map (\ [x,y] -> x) stereo
  return $ keySamples mono

randomN :: Int -> Int -> KeyedSamples -> [(Int, Int32)]
randomN seed n vv = [(U.!) vv i | i<- take n $ randoms $ mkStdGen seed]



