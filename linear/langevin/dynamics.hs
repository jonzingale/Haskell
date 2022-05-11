module Dynamics where
import Linear

type Coords = [Float]
type Dynamics = Coords -> Coords

step :: Float
step = 0.01

d1 :: Dynamics
d1 [x, y, z] = [-y, x, 0]

euler :: Dynamics -> Coords -> Coords
euler dyn xs = xs + step !* dyn xs

rk4 :: Dynamics -> Coords -> Coords
rk4 f xs =
    let k1 = step !* f xs in
    let k2 = step !* (f $ xs + k1 !/ 2) in
    let k3 = step !* (f $ xs + k2 !/ 2) in
    let k4 = step !* (f $ xs + k3) in
    xs + (k1 + 2 !* (k2 + k3) + k4) !/ 6

-- Helpers
showDyn dd =
   -- let it = take 2000 $ iterate (euler dd) [1,0,0] in
   let jt = take 2000 $ iterate (rk4 dd) [1,0,0] in
   putStr.unlines.map show $ jt