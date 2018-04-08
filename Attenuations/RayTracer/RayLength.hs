module RayTracer.RayLength where
import System.Random

{--
TODO:
Find the ray length through a cell.
--}

type Point = (Double, Double) -- valid between 0 and 1
type Slope = (Double, Double)
type Angle = Double

toAngleDeg, toAngleRad :: Slope -> Angle
toAngleDeg (n,d) = toAngleRad (n,d) * 180 / pi
toAngleRad (n,0) = 0
toAngleRad (n,d) = atan (n/d)

{--
Cases:

 εδ γ βα
η_\\|//_η
--}
type RayLength = Point -> Angle -> Double

delta (x,0) theta = 1 / sin theta
beta  (x,0) theta = 1 / sin theta

epsilon (x,0) theta | theta == 0 || theta == pi = 1
                    | otherwise = negate x / cos theta

alpha (x,0) theta | theta == 0 || theta == pi = 1
                  | otherwise = (1-x) / cos theta

-- neither of these are necessary.
eta (x,0) theta | theta == pi || theta == 0 = 1
                | otherwise = 0

gamma (x,0) theta | theta == (pi/2) = 1
                  | otherwise = 0

{--
Conditions:

--}

xregion :: RayLength
xregion cs theta | theta <= pi / 2 = abCondition cs theta
                 | theta <= pi = edCondition cs theta
                 | otherwise = 999 -- a bad value

-- ε-δ transition
edCondition :: RayLength
edCondition (x, 0) theta | cond x theta = delta (x, 0) theta
                         | otherwise = epsilon (x, 0) theta
  where
    cond x t = (pi/2 + x*pi/4) > t

-- α-β transition
abCondition :: RayLength
abCondition (x, 0) theta | cond x theta = alpha (x, 0) theta
                         | otherwise = beta (x, 0) theta
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