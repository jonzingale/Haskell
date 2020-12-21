{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

module Crypto where
import qualified Data.ByteString as BS

{--
check out for elliptics:
http://hackage.haskell.org/package/hecc-0.4.1.1/docs/Codec-Crypto-ECC-Base.html
--}

pdf = "example.pdf"

readText :: FilePath -> IO BS.ByteString
readText file = do
  !s <- BS.readFile file
  return s

ceasar :: Int -> BS.ByteString -> BS.ByteString
ceasar n str =
  let word = fromIntegral n in -- Word8
  BS.map (+ word) str

testCeasarInv :: IO Bool
testCeasarInv = do
  !val <- BS.readFile pdf
  let cs = ceasar (-10) $ ceasar 10 val
  return $ cs == val

saveTmp :: FilePath -> IO ()
saveTmp filename = do
  !str <- BS.readFile filename
  BS.writeFile "tmp.pdf" $ ceasar (-10) str
