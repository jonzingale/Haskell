{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SampleVector where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Data.List (sort)
import System.Random
import Data.WAVE

type Seed = Int
type Size = Int
type Sample = Int32
type KeyedSamples = U.Vector (Int, Int32)

{--
Vectors seem necessary because updating a List is complicated.
Not sure how to sort vectors to conversions are necessary.

*Key Samples as (Int, Int32)
*Vectorize KeyedSamples
*Select subVector
*Convert subVector to List
*Sort List
*update Vector
--}

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

sortSubVector :: Seed -> Size -> KeyedSamples -> KeyedSamples
sortSubVector seed size vv =
  let rs = take (20) $ randomRs (0, size - 1) $ mkStdGen seed in
  let ss = sort $ [(U.!) vv r | r <- rs] in
  let uu = zip rs ss in
  bArray uu vv

iterativeSort :: Size -> KeyedSamples -> [Int32]
iterativeSort size ks = f ks randos size
  where
    randos = randoms $ mkStdGen 23
    f kv (r:rs) size = unkey kv ++ f (sortSubVector r size kv) rs size

testSortSub =
  let vv = keySamples $ reverse [1..100] in
  take (10^6) $ iterativeSort 99 vv

