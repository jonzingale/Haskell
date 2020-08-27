module Main where
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as G
import Codec.Picture -- JuicyPixel
import Conversion -- conversion-1.2.1
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import JsonWriter (writeFilename, writeJson, prismas)

{--
  :set +s

  TODO:
- load image
- iterate a blink while saving images as a side effect
- combine images into movie
--}

filenameStub = "/images/"
-- defaultImage  = "californiaPoppy.jpg"
defaultImage  = "fraggle.jpg"

arguments = do
  args <- getArgs
  let defaults =  ["conway", defaultImage]
  return $ if args == [] then defaults else args

p2d :: Pixel8 -> Double
p2d color = fromIntegral (convert color::Integer)
getRGB (PixelRGB8 r g b) = U.fromList [p2d r, p2d g, p2d b]

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

example :: IO ()
example = do
  directory <- getCurrentDirectory
  image <- getImage (directory ++ "/images/" ++ defaultImage
  let img = convertRGB8 image

main :: IO ()
main = do
  directory <- getCurrentDirectory
  [blinkType, filename] <- arguments

  image <- getImage (directory ++ "/images/" ++ filename)

  let img = convertRGB8 image
  print blinkType
  print $ take 20 $ pixels img
  -- let clusters = kPixelMeans k img
  -- let cents = unWrapAll clusters
  -- let listC = map (U.toList) cents
  -- let formattedC = (closestPrisma ps).(U.toList)
  -- writeJson.map formattedC $ cents
  -- writeFilename (filenameStub ++ filename)

pixels :: Image PixelRGB8 -> [U.Vector Double]
pixels img =
  let (𝜆, width, height) = calculateSizes img in
  [ getRGB $ pixelAt img (𝜆*w) (𝜆*h) | w <- width, h <- height]

calculateSizes :: Image PixelRGB8 -> (Int, [Int], [Int])
calculateSizes img 
  | width*height < 1*10^6 = (1, [0..width - 1], [0..height - 1])
  | otherwise = (𝜆, [0..div (width - 1) 𝜆], [0..div (height - 1) 𝜆])
  where
    -- complexity is quadratic
    𝜆 = (+ 7).floor.maximum.map (sqrt.fromIntegral) $ [width, height]
    [width, height] = [imageWidth img, imageHeight img]
