module Convolve where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import System.Random

type SamplesR = U.Vector Double
type VectSamples = U.Vector Int32

convolve :: SamplesR -> SamplesR -> SamplesR
convolve hs xs =
  let pad = U.replicate ((U.length hs) - 1) 0
      ts  = U.concat pad xs
  in U.map (U.sum . U.zipWith (*) (U.reverse hs)) (U.init $ U.tail ts)
