module Lorenz where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Data.WAVE
import Wave

type ODE = Coords -> Coords
type Coords = (Double, Double, Double)
type Trajectory = [Coords]

{--
This synthesizer is designed to represent
Lorenz equation time series as a tone.
--}

eBall = 0.002 -- both accuracy and center frequency
maxVal = (2^31-1)/3::Double -- averages 3 signals

main = makeWavFile xyzWav
  where
    xyzWav = U.fromList $ map (round.tot) $ runLorenz (1,1,1)
    tot (x,y,z) = x * maxVal * 0.04 +
                  y * maxVal * 0.03 +
                  z * maxVal * 0.02

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

runLorenz :: Coords -> Trajectory
runLorenz cs = take (120*44100) $ -- take 120 gives 2 mins
  iterate (euler lorenz) cs

lorenz :: Coords -> Coords
lorenz (x,y,z) =
  let (σ,β,ρ) = (10, 8/3, 28)
      dx = σ * (y - x)
      dy = x * (ρ - z) - y
      dz = x*y - β*z
  in (dx, dy, dz)
