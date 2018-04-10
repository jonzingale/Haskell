module RayTracer.RayLength where
import System.Random

{--
TODO:
Find the ray length through a cell.
--}

type RayLength = Point -> Angle -> Double
type Point = (Double, Double) -- valid between 0 and 1
type Slope = (Double, Double) -- usually between 0 and π
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

TODO: Revisit the types here, perhaps a switch is what is needed.
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
A good first approximation can be made by determining
which side the ray comes in from and then rotating to
its corresponding x-orientation. x=0 -> 90, x=1 -> 270.

μ ρκ
|//_ι
--}
rho (0,y) theta | theta == pi/2 = 1 -- the mu case
                | otherwise = (1-y) / sin theta

kappa (0,y) theta = 1 / cos theta

-- Rotations
rot270 :: (Point, Angle) -> (Point, Angle) 
rot270 ((x,y), theta) = ((y, 1-x), theta - pi/2)

rot90 :: (Point, Angle) -> (Point, Angle)
rot90 ((x,y), theta) = ((1-y, x), theta + pi/2)

-- Reflections
reflectY :: (Point, Angle) -> (Point, Angle) 
reflectY ((x,y), theta) = ((1-x, y), pi - theta)

{--
Conditions:
--}

yregion :: RayLength
yregion (0,y) theta = (uncurry xregion).rot90  $ ((0,y), theta)
yregion (1,y) theta = (uncurry xregion).rot270 $ ((1,y), theta)
yregion _ _ = 999
