{-# LANGUAGE OverloadedStrings #-}

module Feistel where
import qualified Data.ByteString as BL
import Data.ByteString.Base64 (encode, decode)
import Data.Word
import Data.Bits

{--
:set +s

Todo:
0. Set block size
1. Generate keys of size 1/2 the blockSize, elliptic curve?
--}

type Feistel = [(Text, Text)]
type Text = BL.ByteString
type Key = Text

-- length 42 keys
Right key1 = decode "kTSFoLQRrR+hWJlLjAwXqOH5Z3ZLDWray5mBgNK7lLuHdTwab8m/v96y"
Right key2 = decode "5npvqoiq4jgn4hvwgV6HvWEAGRTV5h7vThEjDsrydgrHSrHtDthrsgrG"
Right key3 = decode "6GWYJR2jbersEgghjJ2esgbnyuMrJEHSRthyNUjesrtThsftHtyHeWsd"

blockSize = 84

encrypt = swap.(feistelRound key1).(feistelRound key2).(feistelRound key3)
decrypt = swap.(feistelRound key3).(feistelRound key2).(feistelRound key1)

feistelRound :: Text -> (Text, Text) -> (Text, Text)
feistelRound k (l, r) = (r, f (f k r) l)
  where f k = BL.pack . BL.zipWith xor k

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

main :: IO Text
main = do
  !f <- BL.readFile "./bigger_text.txt"
  let msgs = chunk blockSize f
  let encrypted = map encrypt msgs
  let decrypted = map decrypt encrypted
  let msg = feistelToText decrypted
  -- print feistelToText encrypted -- test encryption
  -- print (f == msg) -- test invertibility
  return msg

-- break message up into padded ByteStrings of length n (key size)
chunk :: Int -> Text -> Feistel
chunk bsize txt
    | BL.length txt < bsize = [halfs . BL.append txt . pad $ txt]
    | BL.length txt == bsize = [halfs txt]
    | otherwise = (halfs $ BL.take bsize txt) : chunk bsize (BL.drop bsize txt)
    where
      halfs = BL.splitAt (bsize `div` 2)
      pad t = padByteString $ bsize - BL.length t

feistelToText :: Feistel -> Text
feistelToText [] = BL.empty
feistelToText ((a,b):xs) = BL.append (BL.append a b) (feistelToText xs)

padByteString :: Int -> BL.ByteString
padByteString n = BL.pack . take n $ repeat padChar
  where padChar = fromIntegral n :: Word8
