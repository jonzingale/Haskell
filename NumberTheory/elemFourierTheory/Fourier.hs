module Thrush where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Data.WAVE
import Wave

type ODE = Coords -> Coords
type Coords = (Double, Double, Double)
type Trajectory = [Coords]
type Duration = Int
