module RayTracer.RayLength where
import System.Random

{--
TODO:
Find the ray length through a cell.

cases: (as I think of them)
  * should any angle be valid?
--}

points = [(1/4,0), (3/4,0)]
angles = [3*pi/4, 3*pi/8]

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

rayLength' :: Point -> Slope -> Double
rayLength' (x, y) = rayLength (x,y).toAngleRad 

rayLength :: Point -> Angle -> Double
rayLength (x,0) theta | (abs theta) > pi/4 = abs (1/sin theta)
                      | otherwise = abs ((1-x)/cos theta)
rayLength (0,y) theta = rayLength (y,0) theta

{--
Cases:

 εδ γ βα
η_\\|//_η
--}
type RayLength = Point -> Angle -> Double

eta (x,0) theta | theta == pi || theta == 0 = 1
                | otherwise = 0 -- hopefully never this case.

epsilon (x,0) theta | theta == 0 || theta == pi = 1
                    | otherwise = negate x / cos theta

delta (x,0) theta = 1 / sin theta

gamma (x,0) theta | theta == (pi/2) = 1
                  | otherwise = 0

beta  (x,0) theta = 1 / sin theta

alpha (x,0) theta | theta == 0 || theta == pi = 1
                  | otherwise = (1-x) / cos theta

{--
Conditions:

--}

-- ε-δ transition
edCondition :: RayLength
edCondition (x, 0) theta | cond x theta = delta (x, 0) theta
                         | otherwise = epsilon (x, 0) theta
  where
    cond x t = (pi/2 + x*pi/4) > t

-- α-β transition
abCondition :: RayLength
abCondition (x, 0) theta | cond x theta = beta (x, 0) theta
                         | otherwise = alpha (x, 0) theta
  where
    cond x t = (pi/4 + x*pi/4) > t


{--
Cases:

πμρκ
|//_ι
--}

rot270 :: (Point, Angle) -> (Point, Angle) 
rot270 ((x,y), theta) = ((y, 1-x), theta - pi/2)

rot90 :: (Point, Angle) -> (Point, Angle)
rot90 ((x,y), theta) = ((1-y, x), theta + pi/2)