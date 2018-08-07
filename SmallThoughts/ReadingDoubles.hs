{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

module RayTracer.FileToVector (qArray, fileToAry, uArray2D, qArray2D) where
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Vector.Unboxed as U

import qualified Data.ByteString.Lex.Fractional as L
import qualified Data.ByteString.Char8 as L

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

readDouble :: ByteString -> Double
readDouble ls = unsafePerformIO $ B.useAsCString s $ \cstr ->
    realToFrac `fmap` c_strtod cstr nullPtr
  where
    s = B.concat . L.toChunks $ ls

fileToAry2 :: FilePath -> IO (U.Vector Double)
fileToAry2 file = do
!s <- L.readFile file
return.parse $ s
s <- L.readFile file
return $ U.fromList $ readDouble2 s

readDouble2 :: L.ByteString -> [Double]
readDouble2 bs = map (read.(L.unpack)) $ L.lines $ bs

fileToAry3 :: FilePath -> IO (U.Vector Double)
fileToAry3 file = do
  !s <- L.readFile file
  return.parse $ s

parse :: L.ByteString -> U.Vector Double
parse = U.unfoldr step
  where
     step !s = case L.readExponential s of
        Nothing       -> Nothing
        Just (!k, !t) -> Just (k, L.tail t)
 
