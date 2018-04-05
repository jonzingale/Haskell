module RayTracer.RayLength where
import System.Random

{--
TODO:
Find the ray length through a cell.

cases: (as I think of them)
  * should any angle be valid?
--}

type Point = (Double, Double) -- valid between 0 and 1
type Slope = (Double, Double)
type Angle = Double

testRL  = rayLength  (5/14, 0) $ toAngleRad (14,19)
testRL' = rayLength' (5/14, 0) (14,19)
root2 = 1/cos (pi/4)

toAngleDeg, toAngleRad :: Slope -> Angle
toAngleDeg (n,d) = toAngleRad (n,d) * 180 / pi
toAngleRad (n,0) = 0
toAngleRad (n,d) = atan (n/d)

rayLength :: Point -> Angle -> Double
rayLength (x,0) theta | (abs theta) > pi/4 = abs (1/sin theta)
                      | otherwise = abs ((1-x)/cos theta)
rayLength (0,y) theta = rayLength (y,0) theta

rayLength' :: Point -> Slope -> Double
rayLength' (x, y) = rayLength (x,y).toAngleRad 

