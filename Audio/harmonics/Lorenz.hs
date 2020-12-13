module Lorenz where

type ODE = Coords -> Coords
type Coords = (Double, Double, Double)
type Trajectory = [Coords]
type Duration = Int

eBall = 0.0003 -- both accuracy and center frequency

freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

-- normalized x coordinate
example =
  let ls = map ((/ 22).pr1) $ iterate (euler lorenz) (10, 10, 10) in
    concat [take 50 $ repeat x | x <- ls]

example2 =
  let ls = map ((/ 22).pr1) $ iterate (euler lorenz) (10, 11, 10) in
    concat [take 50 $ repeat x | x <- ls]

runLorenz :: Duration -> Coords -> Trajectory
runLorenz sec cs = take (sec*44100) $
  iterate (euler lorenz) cs

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

pr1 :: Coords -> Double
pr1 (x,_,_) = x

lorenz :: Coords -> Coords
lorenz (x,y,z) =
  let (σ,β,ρ) = (10, 8/3, 28)
      dx = σ * (y - x)
      dy = x * (ρ - z) - y
      dz = x*y - β*z
  in (dx, dy, dz)
