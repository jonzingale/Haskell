module Lorenz where

type ODE = Coords -> Coords
type Coords = (Double, Double, Double)
type Trajectory = [Coords]
type Duration = Int

eBall = 0.0003 -- both accuracy and center frequency

freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

-- normalized coordinates
trajX =
  let ls = map ((/ 20).prX) $ iterate (euler lorenz) (10, 10, 10) in
    concat [take 70 $ repeat x | x <- ls]

trajY = -- 
  let ls = map ((/ 27).prY) $ iterate (euler lorenz) (10, 10, 10) in
    concat [take 70 $ repeat x | x <- ls]

trajZ = -- 
  let ls = map ((/ 50).prZ) $ iterate (euler lorenz) (10, 10, 10) in
    concat [take 70 $ repeat x | x <- ls]

prX, prY, prZ :: Coords -> Double
prX (x,_,_) = x
prY (_,y,_) = y
prZ (_,_,z) = z

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

lorenz :: Coords -> Coords
lorenz (x,y,z) =
  let (σ,β,ρ) = (10, 8/3, 28)
      dx = σ * (y - x)
      dy = x * (ρ - z) - y
      dz = x*y - β*z
  in (dx, dy, dz)
