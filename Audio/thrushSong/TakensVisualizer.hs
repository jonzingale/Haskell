module TakensVisualizer where
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as G
import Codec.Picture -- JuicyPixel
import Math.KMeans -- kmeans-vector-0.3.2
import Conversion -- conversion-1.2.1
import Thrush

defaultImage  = "californiaPoppy.jpg"

p2d :: Pixel8 -> Double
p2d color = fromIntegral (convert color::Integer)
getRGB (PixelRGB8 r g b) = U.fromList [p2d r, p2d g, p2d b]


{--
Todo:
- decide how writing images is done with Codec.Picture
- test reconstruction on Lorenz
- attempt reconstruction on thrush song
--}

thing :: IO()
thing = do
  Right image <- readImage defaultImage

  let img = convertRGB8 image
  putStr "done"
