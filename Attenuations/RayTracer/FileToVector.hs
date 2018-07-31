-- https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial

{-# LANGUAGE BangPatterns #-}

module RayTracer.FileToVector (qArray, fileToAry, vLength, vSum,
                               uArray2D, qArray2D, displayRange) where
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector.Unboxed as U
-- import qualified Data.ByteString.Lex.Fractional as L
-- import qualified Data.ByteString.Char8 as L
-- import qualified Data.Vector.Unboxed as U
-- import Data.ByteString.Lazy.Char8 hiding (minimum, maximum, putStr)
import System.Environment

type Dimension = Int -- a single dimension of lattice
type Coords = (Int, Int, Int)
type Coords2D = (Int, Int)

fileToAry :: FilePath -> IO (U.Vector Double)
fileToAry file = do
  s <- L.readFile file
  return $ U.fromList $ readDouble s

readDouble :: L.ByteString -> [Double]
readDouble bs = map (read.(L.unpack)) $ L.lines $ bs

-- fileToAry :: FilePath -> IO (U.Vector Double)
-- fileToAry file = do
--   !s <- L.readFile file
--   return.parse $ s

-- parse :: L.ByteString -> U.Vector Double
-- parse = U.unfoldr step
--   where
--      step !s = case L.readExponential s of
--         Nothing       -> Nothing
--         Just (!k, !t) -> Just (k, L.tail t)

uArray2D :: U.Unbox a => Dimension -> Coords2D -> a -> U.Vector a -> U.Vector a
uArray2D size (x, y) v a = (U.//) a [(x + y * size, v)]

qArray2D :: U.Unbox a => Dimension -> Coords2D -> U.Vector a -> a
qArray2D size (x, y) a = (U.!) a (x + y * size)

qArray :: U.Unbox a => Dimension -> Coords -> U.Vector a -> a
-- qArray size (x, y, z) a = (U.!) a (x + y * size * size + z * size)
qArray size (x, y, z) a = (U.!) a (x + z * size * size + y * size)

vLength :: U.Unbox a => U.Vector a -> Int
vLength = U.length

vSum :: (Num b, U.Unbox b) => U.Vector b -> b
vSum = U.sum

displayRange :: U.Vector Double -> IO ()
displayRange vect = do
  let ary = (U.toList) vect
  let (s, l) = (show.minimum $ ary, show.maximum $ ary)
  putStr $ "\nminimum: " ++ s ++ "\nmaximum: " ++ l ++ "\n"