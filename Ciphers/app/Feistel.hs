{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Feistel where
import qualified Data.ByteString as B
import Data.ByteString.Base64 (decode)
import Data.Word
import Data.Bits

{--
:set +s

Todo:
1. Generate keys of size 1/2 the blockSize, elliptic curve?
2. Better padding:
  a. get length of vector
  b. add block sized junk
  c. take length of vector

Sun Dec  4 12:30 2022 Time and Allocation Profiling Report  (Final)

  Ciphers +RTS -p -s -N4 -hT -i0.1 -RTS

  total time  =        0.24 secs   (948 ticks @ 1000 us, 4 processors)
  total alloc = 31,534,292,952 bytes  (excludes profiling overheads)

COST CENTRE            MODULE  SRC                            %time %alloc

feistelRound.f         Feistel app/Feistel.hs:59:9-42          77.2    0.6
feistelToText.fuseBody Feistel app/Feistel.hs:65:5-32          17.7   99.2
feistelRound           Feistel app/Feistel.hs:(58,1)-(59,42)    2.4    0.0
--}

type Feistel = [(Text, Text)]
type Text = B.ByteString
type Key = Text

-- length 64 keys
key1, key2, key3 :: Text
Right key1 = decode "kTSFoLQRrR+hWJlLjAwXqOH5Z3ZLDWray5mBgNK7lLuHdTwab8m/v96ykTSFoLQR"
Right key2 = decode "5npvqoiq4jgn4hvwgV6HvWEAGRTV5h7vThEjDsrydgrHSrHtDthrsgrykTSFoLQR"
Right key3 = decode "6GWYJR2jbersEgghjJ2esgbnyuMrJEHSRthyNUjesrtThsftHtyHeWsykTSFoLQR"

keys :: [Text]
keys = [key3, key2, key1]

blockSize :: Int
blockSize = 64

encrypt, decrypt :: (Text, Text) -> (Text, Text)
encrypt = foldr (.) swap $ map feistelRound keys
decrypt = foldr (.) swap $ map feistelRound $ reverse keys

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

feistelRound :: Text -> (Text, Text) -> (Text, Text)
feistelRound k (l, r) = (r, f (f k r) l)
  where f k' = B.pack . B.zipWith xor k'

feistelToText :: Feistel -> Text
feistelToText xs = foldr (fuseBody.fuseHead) B.empty xs
  where
    fuseHead (p, q) = B.append p q
    fuseBody a b = B.append a b

-- break message up into padded ByteStrings of length n (key size)
chunk :: Int -> Text -> [(Text, Text)]
chunk bsize txt
    | B.length txt < bsize = [halfs . B.append txt . pad $ txt]
    | B.length txt <= bsize = [halfs txt]
    | otherwise = (halfs $ B.take bsize txt) : chunk bsize (B.drop bsize txt)
    where
      halfs = B.splitAt (bsize `div` 2)
      pad t = padByteString $ bsize - B.length t

padByteString :: Int -> B.ByteString
padByteString n = B.pack . take n $ repeat padChar
  where padChar = fromIntegral n :: Word8
