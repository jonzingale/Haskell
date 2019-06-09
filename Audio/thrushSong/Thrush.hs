module Thrush where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Data.WAVE
import Wave

type ODE = Coords -> Coords
type Coords = (Double, Double, Double)
type Trajectory = [Coords]
type Duration = Int

{--
This synthesizer is designed to represent
Lorenz equation time series as a tone.
--}

eBall = 0.003 -- both accuracy and center frequency
maxVal = (2^31-1)/3::Double -- averages 3 signals

main = makeStereoWavFile left right
  where
    cs = (1,1,1)
    left = U.fromList $ map (round.lf) $ runLorenz 120 cs
    right = U.fromList $ map (round.rf) $ runLorenz 120 cs
    lf (x,y,z) = x * maxVal * 0.04 + 0.4 * y * maxVal * 0.03
    rf (x,y,z) = z * maxVal * 0.02 + 0.4 * y * maxVal * 0.03

euler :: ODE -> Coords -> Coords
euler f (x,y,z) =
    let (s,t,r) = f (x,y,z)
        dx = x + s * eBall
        dy = y + t * eBall
        dz = z + r * eBall
        (s',t',r') = f (dx, dy, dz)
        ddx = dx + s' * eBall
        ddy = dy + t' * eBall
        ddz = dz + r' * eBall
    in ((dx + ddx) /2.0, (dy + ddy) /2.0,  (dz + ddz) /2.0)

runLorenz :: Duration -> Coords -> Trajectory
runLorenz sec cs = take (sec*44100) $
  iterate (euler lorenz) cs

lorenz :: Coords -> Coords
lorenz (x,y,z) =
  let (σ,β,ρ) = (10, 8/3, 28)
      dx = σ * (y - x)
      dy = x * (ρ - z) - y
      dz = x*y - β*z
  in (dx, dy, dz)
