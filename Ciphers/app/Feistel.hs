{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Feistel where
import qualified Data.ByteString as BL
import Data.ByteString.Base64 (decode)
import qualified Data.Vector as U
import Data.Word
import Data.Bits

{--
:set +s

Todo:
0. Set block size
1. Generate keys of size 1/2 the blockSize, elliptic curve?
2. Parallelize vector work
3. Better padding:
  a. get length of vector
  b. add block sized junk
  c. take length of vector

COST CENTRE            MODULE SRC                         %time %alloc

feistelToText.fuseBody Main   app/Main.hs:75:5-32          71.0   96.7
feistelRound.f         Main   app/Main.hs:43:9-40          17.5    2.3
main                   Main   app/Main.hs:(49,1)-(57,11)    7.9    1.0
MAIN                   MAIN   <built-in>                    2.3    0.0

NOTES:
1. compiled speeds are already quite a bit better than interpreted
2. print (f== msg) only True when blockSize = 84 and file = "./app/text.txt"
--}

type Feistel = U.Vector (Text, Text)
type Text = BL.ByteString
type Key = Text

-- length 42 keys
key1, key2, key3 :: BL.ByteString
Right key1 = decode "kTSFoLQRrR+hWJlLjAwXqOH5Z3ZLDWray5mBgNK7lLuHdTwab8m/v96y"
Right key2 = decode "5npvqoiq4jgn4hvwgV6HvWEAGRTV5h7vThEjDsrydgrHSrHtDthrsgrG"
Right key3 = decode "6GWYJR2jbersEgghjJ2esgbnyuMrJEHSRthyNUjesrtThsftHtyHeWsd"

blockSize :: Int
blockSize = 84 -- 64?

encrypt, decrypt :: (Text, Text) -> (Text, Text)
encrypt = swap.(feistelRound key1).(feistelRound key2).(feistelRound key3)
decrypt = swap.(feistelRound key3).(feistelRound key2).(feistelRound key1)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

feistelRound :: Text -> (Text, Text) -> (Text, Text)
feistelRound k (l, r) = (r, f (f k r) l)
  where f k' = BL.pack . BL.zipWith xor k'

-- COST CENTRE            MODULE SRC                         %time %alloc
-- feistelToText.fuseBody Main   app/Main.hs:75:5-32          71.0   96.7
feistelToText :: Feistel -> Text
feistelToText xs = U.foldr (fuseBody.fuseHead) BL.empty xs
  where
    fuseHead (p, q) = BL.append p q
    fuseBody a b = BL.append a b

-- break message up into padded ByteStrings of length n (key size)
chunk :: Int -> Text -> [(Text, Text)]
chunk bsize txt
    | BL.length txt < bsize = [halfs . BL.append txt . pad $ txt]
    | BL.length txt <= bsize = [halfs txt]
    | otherwise = (halfs $ BL.take bsize txt) : chunk bsize (BL.drop bsize txt)
    where
      halfs = BL.splitAt (bsize `div` 2)
      pad t = padByteString $ bsize - BL.length t

padByteString :: Int -> BL.ByteString
padByteString n = BL.pack . take n $ repeat padChar
  where padChar = fromIntegral n :: Word8
