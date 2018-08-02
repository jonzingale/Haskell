-- https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial

module RayTracer.FileToVector (qArray, fileToAry, uArray2D, qArray2D) where
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector.Unboxed as U
import System.Environment

type Dimension = Int -- single side of lattice
type Coords = (Int, Int, Int)
type Coords2D = (Int, Int)

fileToAry :: FilePath -> IO (U.Vector Double)
fileToAry file = do
  s <- L.readFile file
  return $ U.fromList $ readDouble s

readDouble :: L.ByteString -> [Double]
readDouble bs = map (read.(L.unpack)) $ L.lines $ bs

uArray2D :: U.Unbox a => Dimension -> Coords2D -> a -> U.Vector a -> U.Vector a
uArray2D size (x, z) v a = (U.//) a [(x + z * size, v)]

qArray2D :: U.Unbox a => Dimension -> Coords2D -> U.Vector a -> a
qArray2D size (x, y) a = (U.!) a (x + y * size)

qArray :: U.Unbox a => Dimension -> Coords -> U.Vector a -> a
qArray size (x, y, z) a = (U.!) a (x + y * size + z * size * size)
