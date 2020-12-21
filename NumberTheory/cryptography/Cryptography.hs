{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Cryptography where
import Data.ByteString as BS hiding (concat, map, take)
import Data.ByteString.Conversion (fromByteString, toByteString)
import Ceasar (ceasar)
{--
check out for elliptics:
http://hackage.haskell.org/package/hecc-0.4.1.1/docs/Codec-Crypto-ECC-Base.html

:set +s
--}

pdf = "example.pdf"

readText :: FilePath -> IO BS.ByteString
readText file = do
  !s <- BS.readFile file
  return s

saveTmp :: FilePath -> IO ()
saveTmp filename = do
  !str <- BS.readFile filename
  BS.writeFile "tmp.pdf" $ ceasar (-10) str

-- make sure to pad these [0..255]
oneBigNum :: BS.ByteString -> Integer
oneBigNum bs = read.concat.map show $ unpack bs

toListNums :: BS.ByteString -> [Integer]
toListNums bs = map fromIntegral $ unpack bs
