{-# LANGUAGE BangPatterns #-}

module Main where
import Control.Parallel.Strategies (rdeepseq, parListChunk, using)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Vector as U
import qualified Feistel as F
import qualified RSA as RSA
import Data.Char

-- main :: IO ()
-- main = do
--   !f <- BL.readFile "./app/text.txt"
--   let msg = (map fromIntegral $ BL.unpack f) :: [Integer] -- padding scheme
--   let demsg = RSA.decrypt . RSA.encrypt $ U.fromList msg
--   putStrLn $ U.toList $ U.map (chr.fromIntegral) demsg

main :: IO F.Text
main = do
  !f <- B.readFile "./app/bigger_text.txt"
  let len = B.length f
  let msgs = F.chunk F.blockSize $ f
  let encrypted = map F.encrypt msgs
  let resultE = encrypted `using` parListChunk 4096 rdeepseq
  let decrypted = map F.decrypt resultE
  let resultD = decrypted `using` parListChunk 4096 rdeepseq
  let padded_msg = F.feistelToText resultD
  let msg = B.take len padded_msg
  print (f == msg)
  -- print (len, B.length msg)
  -- print $ B.take 128 msg
  -- print $ B.take 128 f
  -- print msg
  return msg -- for useful return
