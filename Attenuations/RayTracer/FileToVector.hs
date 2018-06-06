
{-# LANGUAGE BangPatterns #-}
module RayTracer.FileToVector (qArray, fileToAry, vLength, vSum) where
import qualified Data.ByteString.Lex.Fractional as L
import qualified Data.ByteString.Char8 as L
import qualified Data.Vector.Unboxed as U
import System.Environment

{--
Vector parsing via:
https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial

optimize at compilation time:
$ ghc -Odph --make FileToVector.hs
$ time ./FileToVector
--}

-- The basic idea:
main = do
  ary <- fileToAry "./Tests/data1M"
  return $ qArray 1000 (20, 300, 0) ary

parse :: L.ByteString -> U.Vector Double
parse = U.unfoldr step
  where
     step !s = case L.readExponential s of
        Nothing       -> Nothing
        Just (!k, !t) -> Just (k, L.tail t)

type Dimension = Int -- a single dimension of lattice
type Coords = (Int, Int, Int)
type Coords2D = (Int, Int)

-- The following methods perhaps part of type class?
fileToAry :: FilePath -> IO (U.Vector Double)
fileToAry file = do
  !s <- L.readFile file
  return.parse $ s

qArray2D :: U.Unbox a => Dimension -> Coords2D -> U.Vector a -> a
qArray2D size (x, y) a = (U.!) a (x + y * size)

-- How should this work?
qArray :: U.Unbox a => Dimension -> Coords -> U.Vector a -> a
qArray size (x, y, z) a = (U.!) a (x + y * size)

vLength :: U.Unbox a => U.Vector a -> Int
vLength = U.length

vSum :: (Num b, U.Unbox b) => U.Vector b -> b
vSum = U.sum
