module Lorenz where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import System.Random
import Data.WAVE
import Wave

type Coords = (Double, Double, Double)
type Trajectory = [Coords]

{--
dx = σ (y - x)
dy = x (ρ - z) - y
dz = xy - βz
--}
eBall = 0.002 -- both accuracy and center frequency
maxVal = (2^31-1)/3::Double -- averages 3 signals

-- main (1,3,1)
main cs = makeWavFile $ xyzWav cs
  where
    xyzWav cs = U.fromList $ map (round.tot) $ runLorenz cs
      where
        tot (x,y,z) = x * maxVal * 0.04 +
                      y * maxVal * 0.03 +
                      z * maxVal * 0.02

improvedEuler :: (Coords -> Coords) -> Coords -> Coords
improvedEuler f (x,y,z) =
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
runLorenz cs = take (120*44100) $ iterate (improvedEuler lorenz) cs

lorenz :: Coords -> Coords
lorenz (x,y,z) =
  let (σ,β,ρ) = (10, 8/3, 28)
      dx = σ * (y - x)
      dy = x * (ρ - z) - y
      dz = x*y - β*z
  in (dx, dy, dz)

----

euler :: (Coords -> Coords) -> Coords -> Coords
euler f (x,y,z) =
  let (s,t,r) = f (x,y,z)
      dx = x + s * eBall
      dy = y + t * eBall
      dz = z + r * eBall
  in (dx, dy, dz)