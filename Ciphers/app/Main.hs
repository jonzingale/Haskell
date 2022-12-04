{-# LANGUAGE BangPatterns #-}

module Main where
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
  let msgs = U.fromList . F.chunk F.blockSize $ f
  let encrypted = U.map F.encrypt msgs
  let decrypted = U.map F.decrypt encrypted
  let msg = F.feistelToText decrypted
  test1 f msg
  -- test2 encrypted
  test2 decrypted
  test3 msg
  return msg -- for useful return
  where
    -- print (f== msg) only True when blockSize = 84 and file = "./app/text.txt"
    test1 f' m = print (f' == m) -- test invertibility, main :: IO ()
    test2 e = print $ F.feistelToText e -- test encryption
    test3 m = print $ B.head m -- for profiling, main :: IO Word8