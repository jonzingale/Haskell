-- https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial

module RayTracer.FileToVector (qArray, fileToAry, uArray2D, qArray2D) where
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector.Unboxed as U

type Lattice = U.Vector Double
type Coords = (Int, Int, Int)
type Coords2D = (Int, Int)
type Dimension = Int

fileToAry :: FilePath -> IO Lattice
fileToAry file = do
  s <- L.readFile file
  return $ U.fromList $ readDouble s

readDouble :: L.ByteString -> [Double]
readDouble bs = map (read.(L.unpack)) $ L.lines $ bs

uArray2D :: Dimension -> Coords2D -> Double -> Lattice -> Lattice
uArray2D size (x, z) v a = (U.//) a [(x + z * size, v)]

qArray2D :: Dimension -> Coords2D -> Lattice -> Double
qArray2D size (x, y) a = (U.!) a (x + y * size)

qArray :: Dimension -> Coords -> Lattice -> Double
qArray size (x, y, z) a = (U.!) a (x + y * size + z * size * size)
