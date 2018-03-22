module Test where
import Graphics.Ascii.Haha.Plot
import Graphics.Ascii.Haha.Geometry
import Graphics.Ascii.Haha.Terminal
import Graphics.Ascii.Haha.Bitmap
{--
data Pixel Source

Constructors : Pixel Char String

Instances:
Eq Pixel   
Show Pixel

orderPoint :: Ord t => Point t -> Point t -> Ordering -- LT, GT etc..
list :: (Integral i, RealFrac u) => u -> Rect u -> Bitmap u p -> [(Point i, p)]
string :: (Integral i, Show i) => Bool -> Rect i -> Point i -> String -> String -> [(Point i, Pixel)] -> String
--}

circle = Circle pt (3.0)
rect = Rect (Point 2 2) (Point 4 7)
pxl = Pixel '2' "this"
pt = Point 2 4

bmp = put pt "S" empty

drawC = drawCircle circle 3.0 "s" bmp
