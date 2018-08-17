module RayTracer.RayLengthHelpers (rot270, rot90, reflectY, yregion') where
import RayTracer.RayLength

-- radian-degree conversion
toAngleDeg, toAngleRad :: Slope -> Angle
toAngleDeg (n,d) = toAngleRad (n,d) * 180 / pi
toAngleRad (n,0) = 0
toAngleRad (n,d) = atan (n/d)

-- Rotations
rot270 :: (Point, Angle) -> (Point, Angle) 
rot270 ((x,y), theta) = ((y, 1-x), theta - pi/2)

rot90 :: (Point, Angle) -> (Point, Angle)
rot90 ((x,y), theta) = ((1-y, x), theta + pi/2)

-- Reflections
reflectY :: (Point, Angle) -> (Point, Angle) 
reflectY ((x,y), theta) = ((1-x, y), pi - theta)

yregion' :: RayLength -- transforms y propblem to be an x problem
yregion' (0,y) theta = (uncurry xregion).rot90  $ ((0,y), theta)
yregion' (1,y) theta = (uncurry xregion).rot270 $ ((1,y), theta)
yregion' _ _ = 999
