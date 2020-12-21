{-# LANGUAGE BangPatterns #-}

module Ceasar where
import qualified Data.ByteString as BS

ceasar :: Int -> BS.ByteString -> BS.ByteString
ceasar n str =
  let word = fromIntegral n in -- Word8
  BS.map (+ word) str

testCeasarInv :: IO Bool
testCeasarInv = do
  !val <- BS.readFile "example.pdf"
  let cs = ceasar (-10) $ ceasar 10 val
  return $ cs == val
