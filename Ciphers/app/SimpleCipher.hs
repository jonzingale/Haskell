{-# LANGUAGE OverloadedStrings #-}

module SimpleCipher where
import Data.ByteString.Char8 ()
import qualified Data.ByteString as B
import Data.ByteString.Base64 (decode)
import Data.Bits -- (xor)

-- length 42, 4 * 10 + 2
Right key = decode "kTSFoLQRrR+hWJlLjAwXqOH5Z3ZLDWray5mBgNK7lLuHdTwab8m/v96y"
-- length <= 42
message = "I'm a secret message 1234567890"

encrypt = B.pack . B.zipWith xor key
decrypt = encrypt

main = do
  let encrypted = encrypt message
  print encrypted
  print $ decrypt encrypted