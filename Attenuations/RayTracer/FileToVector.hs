-- https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial

{-# LANGUAGE ForeignFunctionInterface #-}

module RayTracer.FileToVector (qArray, fileToAry, uArray2D, qArray2D) where
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Vector.Unboxed as U

import Foreign
import Foreign.C.Types
import System.IO.Unsafe

type Lattice = U.Vector Double
type Coords = (Int, Int, Int)
type Coords2D = (Int, Int)
type Dimension = Int

foreign import ccall unsafe "static stdlib.h strtod" c_strtod
    :: Ptr CChar -> Ptr (Ptr CChar) -> IO CDouble

fileToAry :: FilePath -> IO Lattice
fileToAry file = do
  s <- L.readFile file
  return $ U.fromList $ map readDouble $ L.lines s

readDouble :: ByteString -> Double
readDouble ls = unsafePerformIO $ B.useAsCString s $ \cstr ->
    realToFrac `fmap` c_strtod cstr nullPtr
  where
    s = B.concat . L.toChunks $ ls

uArray2D :: Dimension -> Coords2D -> Double -> Lattice -> Lattice
uArray2D size (x, z) v a = (U.//) a [(x + z * size, v)]

qArray2D :: Dimension -> Coords2D -> Lattice -> Double
qArray2D size (x, y) a = (U.!) a (x + y * size)

qArray :: Dimension -> Coords -> Lattice -> Double
qArray size (x, y, z) a = (U.!) a (x + y * size + z * size * size)
