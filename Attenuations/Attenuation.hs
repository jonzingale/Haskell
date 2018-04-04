module Attenuation where
import System.Random
import Data.Array
{--
TODO:
Make a 1000 cubic-cell mesh and calculate the path sums through
the mesh with a function at every point. The value of the function
gives the amount of attenuation through the mesh.

Further, the length of the ray through each given
cell is needed to compute the value at the given cell.

10^9 cells eventually.
--}

{--
Given position (x,y) and incidence (θ,φ)
return a list of indices through the mesh.
--}

type Coords = Array Integer Double
data BigArray = VLA {pr1::Coords, pr2::Coords, pr3::Coords} deriving Show

-- rArray takes seed values to produce random arrays.
bigArray = VLA (rArray 21) (rArray 42) (rArray 23)
  where
    rArray = listArray (0,100).randomRs (0,10^3).mkStdGen

rabbits (n,d) = f n d 0 0
  where
    f n d i j | n*i < d*j = 'L' : f n d (i+1) j
              | n*i > d*j = 'r' : f n d i (j+1)
              | otherwise = '.' : f n d 1 1

-- Find the ray length through a cell.
type Point = (Double, Double)
type Slope = (Double, Double)
type Angle = Double

testRL  = rayLength  (5/14, 0) $ toAngleRad (14,19)
testRL' = rayLength' (5/14, 0) (14,19)
root2 = 1/cos (pi/4)

toAngleDeg, toAngleRad :: Slope -> Angle
toAngleDeg (n,d) = atan (n/d) * 180 / pi
toAngleRad (n,d) = atan (n/d)

rayLength :: Point -> Angle -> Double -- needs negative angle cases.
rayLength (x, y) theta | abs theta <= (pi/4) = (1-x)/(cos theta)
                       | otherwise = (1-x)/(sin theta)

rayLength' :: Point -> Slope -> Double
rayLength' (x, y) = rayLength (x,y).toAngleRad 




