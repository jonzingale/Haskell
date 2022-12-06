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
2. Better padding

Mon Dec  5 20:50 2022 Time and Allocation Profiling Report  (Final)

  Ciphers +RTS -p -s -N4 -hT -RTS

  total time  =        0.10 secs   (397 ticks @ 1000 us, 4 processors)
  total alloc = 32,149,775,880 bytes  (excludes profiling overheads)

COST CENTRE            MODULE  SRC                            %time %alloc

feistelToText.fuseBody Feistel app/Feistel.hs:65:5-31          49.1   97.3
feistelRound.f         Feistel app/Feistel.hs:59:9-40          26.4    1.8
main                   Main    app/Main.hs:(20,1)-(35,12)      10.3    0.7
decrypt                Feistel app/Feistel.hs:52:1-58           4.8    0.0
encrypt                Feistel app/Feistel.hs:51:1-48           3.5    0.0
feistelRound           Feistel app/Feistel.hs:(58,1)-(59,40)    2.3    0.0
chunk                  Feistel app/Feistel.hs:(69,1)-(75,48)    1.5    0.0
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
