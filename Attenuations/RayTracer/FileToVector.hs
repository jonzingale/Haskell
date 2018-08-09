-- https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

module RayTracer.FileToVector (fileToAry, qArray, qArray2D) where
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector.Unboxed as U
-- import qualified Data.ByteString as S

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
  !s <- L.readFile file
  return $ U.fromList $ map readDouble $ L.lines s

readDouble :: L.ByteString -> Double
readDouble ls = unsafePerformIO $ B.useAsCString s $ \cstr ->
    realToFrac `fmap` c_strtod cstr nullPtr
  where
    s = B.concat . L.toChunks $ ls
    -- s = B.concat $ go 0 (L.toChunks  ls)

-- bsize = 2048 * 1024 * 1024
-- go :: Int -> [S.ByteString] -> Int
-- go s (c:cs) | s >= bsize = s
--             | otherwise = go (s + S.length c) cs

qArray2D :: Dimension -> Coords2D -> Lattice -> Double
qArray2D size (x, y) a = (U.!) a (x + y * size)

qArray :: Dimension -> Coords -> Lattice -> Double
qArray size (x, y, z) a = (U.!) a (x + y * size + z * size * size)
