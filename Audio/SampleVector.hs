module SampleVector where
import qualified Data.Vector.Unboxed as U
import Data.List.Split (divvy)
import Data.Int (Int32)
import Data.List (sort)
import System.Random
import SortsShuffles
import Data.WAVE

type Seed = Int
type Size = Int
type Sample = Int32
type KeyedSamples = U.Vector (Int, Int32)

unkey :: KeyedSamples -> [Int32]
unkey ks = snd.unzip $ U.toList $ ks

-- update
uArray :: U.Unbox a => Int -> a -> U.Vector a -> U.Vector a
uArray t v a = (U.//) a [(t, v)]

-- query
qArray :: U.Unbox a => Int -> U.Vector a -> a
qArray t a = (U.!) a t

-- bulk update
bArray :: U.Unbox a => [(Int, a)] -> U.Vector a -> U.Vector a
bArray tvs a = (U.//) a tvs

sortSubVector :: Seed -> Size -> KeyedSamples -> KeyedSamples
sortSubVector seed size vv =
  let rs = take (div size 2) $ seedShuffle seed [0..size-1] in
  let ks = sort [(U.!) vv r | r <- rs] in
  let uu = zip (sort rs) ks in
  bArray uu vv

iterativeSort :: Size -> KeyedSamples -> [Int32]
iterativeSort size ks = f ks randos size
  where
    randos = randoms $ mkStdGen 31
    f kv (r:rs) size = unkey kv ++ f (sortSubVector r size kv) rs size

testSortSub = do
  let (size, size') = (10::Int, 10::Int32)
  let vect = U.fromList $ shuffle $ zip [0..] $ map negate [1..size']
  let chunks = divvy size size $ iterativeSort size vect
  putStr.unlines.map show $ chunks
