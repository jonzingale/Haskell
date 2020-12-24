{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Cryptography where
import Data.ByteString as BS hiding (concat, map, take, length)
import Data.ByteString.Conversion (fromByteString, toByteString)
import Ceasar (ceasar)
import File (pdf)
{--
check out for elliptics:
http://hackage.haskell.org/package/hecc-0.4.1.1/docs/Codec-Crypto-ECC-Base.html

:set +s
--}

-- also defined in File.hs
saveTmp :: FilePath -> IO ()
saveTmp filename = do
  !str <- BS.readFile filename
  BS.writeFile "tmp.pdf" $ ceasar (-10) str

-- Careful when converting to Integer: '037123' -> 37123
-- Probably good to write as a class/API.
oneBigNum :: BS.ByteString -> Integer
oneBigNum bs = read.concat.map (padding.show).unpack $ bs
  where -- pad vals [0..255]
    padding (x:y:z:[]) = [x,y,z]
    padding (x:y:[]) = '0':[x,y]
    padding (x:[]) = '0':'0':[x]

toListNums :: BS.ByteString -> [Integer]
toListNums bs = map fromIntegral $ unpack bs
