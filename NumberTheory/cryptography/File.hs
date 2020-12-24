{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module File where
import Data.ByteString as BS hiding (concat, map, take, length)
import Data.ByteString.Conversion (fromByteString, toByteString)
import Ceasar (ceasar)

pdf = "example.pdf"

readText :: FilePath -> IO BS.ByteString
readText file = do
  !s <- BS.readFile file
  return s

saveTmp :: FilePath -> IO ()
saveTmp filename = do
  !str <- BS.readFile filename
  BS.writeFile "tmp.pdf" $ ceasar (-10) str

chunking :: BS.ByteString -> [BS.ByteString]
chunking = undefined