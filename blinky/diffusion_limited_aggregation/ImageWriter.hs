module ImageWriter where
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as G
import Codec.Picture -- JuicyPixel
import Conversion -- conversion-1.2.1

{--
  :set +s

TODO:
- load image
- iterate a DLA while saving images as a side effect
- combine images into movie
--}

filenameStub = "/images/"
defaultImage  = "insideACell.jpg"

imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

-- WARNING: Because of the PNG memory leak, see kPixelMeans below,
-- DO NOT rely on PNG conversion, use only proper JPGs.
-- attempts to produce jpeg then tries to produce png
getImage :: FilePath -> IO DynamicImage
getImage filepath = do
  image <- readJpeg filepath
  case image of
    Right chet -> return chet
    Left chet -> do
      Right chet <- readPng filepath
      return chet

pixels :: Image PixelRGB8 -> [U.Vector Double]
pixels img =
  let (𝜆, width, height) = calculateSizes img in
  [ getRGB $ pixelAt img (𝜆*w) (𝜆*h) | w <- width, h <- height]

getRGB :: PixelRGB8 -> U.Vector Double
getRGB (PixelRGB8 r g b) = U.fromList [p2d r, p2d g, p2d b]
  where
    p2d :: Pixel8 -> Double
    p2d color = fromIntegral (convert color::Integer)

calculateSizes :: Image PixelRGB8 -> (Int, [Int], [Int])
calculateSizes img 
  | width*height < 1*10^6 = (1, [0..width - 1], [0..height - 1])
  | otherwise = (𝜆, [0..div (width - 1) 𝜆], [0..div (height - 1) 𝜆])
  where
    -- complexity is quadratic
    𝜆 = (+ 7).floor.maximum.map (sqrt.fromIntegral) $ [width, height]
    [width, height] = [imageWidth img, imageHeight img]
