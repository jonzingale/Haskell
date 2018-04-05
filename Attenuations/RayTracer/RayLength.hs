module RayTracer.RayLength where
import System.Random

-- Find the ray length through a cell.
type Point = (Double, Double)
type Slope = (Double, Double)
type Angle = Double

testRL  = rayLength  (5/14, 0) $ toAngleRad (14,19)
testRL' = rayLength' (5/14, 0) (14,19)
root2 = 1/cos (pi/4)

toAngleDeg, toAngleRad :: Slope -> Angle
toAngleDeg (n,d) = toAngleRad (n,d) * 180 / pi
toAngleRad (n,d) | d==0 = 0
                 | otherwise = atan (n/d)

rayLength :: Point -> Angle -> Double -- needs negative angle cases.
rayLength (x, y) theta | abs theta <= (pi/4) = (1-x)/(cos theta)
                       | otherwise = (1-x)/(sin theta)

rayLength' :: Point -> Slope -> Double
rayLength' (x, y) = rayLength (x,y).toAngleRad 




