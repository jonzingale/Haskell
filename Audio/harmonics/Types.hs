module Types where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)

type VectSamples = U.Vector Int32
type Sound = (String, Duration)
type Timbre = Double -> [Double]
type DurationSecs = Double
type Epoch = Duration
type Freq = Double

data Duration =
  Eighth | EighthD |
  Quarter | QuarterD |
  Half | HalfD |
  Whole | WholeD deriving (Show, Eq, Ord)
