module Line where

{--
TODO:
module for tracing color
module for thickening lines and dots
module for bspline interpolation
--}

{--
functionality for simple line interpolation
--}

type Point = (Int, Int)
type Param = Double
type Density = Int

interpolate :: Point -> Point -> Param -> Point
interpolate (x1, y1) (x2, y2) t =
  g (f x1 + t * f (x2 - x1), f y1 + t * f (y2 - y1))
  where
    f x = fromIntegral x :: Double
    g (x, y) = (floor x, floor y) 

line :: Point -> Point -> Density -> [Point]
line x y d =
  [interpolate x y (fromIntegral t / fromIntegral d) | t <- [0..d-1]]

drawLine :: Density -> [Point] -> [Point]
drawLine d [] = []
drawLine d [x] = []
drawLine d (x:y:xs) = line x y d ++ drawLine d (y:xs)