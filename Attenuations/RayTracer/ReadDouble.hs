{-# LANGUAGE ForeignFunctionInterface #-}

module RayTracer.ReadDouble where
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString as SB
import Data.ByteString.Internal (inlinePerformIO)
import Foreign.C.String (CString)
import Foreign.C (CDouble)
import Data.Maybe (fromJust)

foreign import ccall unsafe "stdlib.h atof" c_atof :: CString -> IO Double
unsafeReadDouble = inlinePerformIO . flip SB.useAsCString c_atof

readDouble = unsafeReadDouble . SB.concat . LB.toChunks
readInt = fst . fromJust . LB.readInt


-- main = LB.getContents >>= (print . sum . map readDouble . LB.lines)