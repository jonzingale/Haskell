{-# LANGUAGE BangPatterns #-}

module RayTracer.PhotographicPlate where
import qualified Data.ByteString.Lex.Fractional as L
import qualified Data.ByteString.Char8 as L
import qualified Data.Vector.Unboxed as U
import System.Environment

type Dimension = Int -- length of lattice side
type Coords = (Int, Int, Int)
type Coords2D = (Int, Int)

type Attenuation = Double
type Distance = Double

{--
Here there should be a method for averaging
the rays and returning a UArray to publish
as a File.
--}

getAry = do
  ary <- fileToAry "./Tests/data1M" -- :: U.Vector Double
  putStr.show $ qArray2D 1000 (0,0) ary
  let bry = uArray2D 1000 (0,0) 1.0 ary
  putStr $ "\n" ++ show (qArray2D 1000 (0,0) bry) ++ "\n"

uArray2D :: U.Unbox a => Dimension -> Coords2D -> a -> U.Vector a -> U.Vector a
uArray2D size (x, y) v a = (U.//) a [(x + y * size, v)]

qArray2D :: U.Unbox a => Dimension -> Coords2D -> U.Vector a -> a
qArray2D size (x, y) a = (U.!) a (x + y * size)

-- qArray :: U.Unbox a => Dimension -> Coords -> U.Vector a -> a
-- qArray size (x, y, z) a = (U.!) a (x + y * size * size + z * size)

-- File To Vector
fileToAry :: FilePath -> IO (U.Vector Double)
fileToAry file = do
  !s <- L.readFile file
  return.parse $ s

parse :: L.ByteString -> U.Vector Double
parse = U.unfoldr step
  where
     step !s = case L.readExponential s of
        Nothing       -> Nothing
        Just (!k, !t) -> Just (k, L.tail t)

