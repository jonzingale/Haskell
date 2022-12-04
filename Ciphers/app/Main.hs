{-# LANGUAGE BangPatterns #-}

module Main where
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as U
import Data.Char
import RSA

{--
https://www.schoolofhaskell.com/user/tekul/bytestring-bits-and-pieces

c = m^e `mod` m
d = c^(inv e) `mod` m
(a^e)^d == (a^e)(a^e)..d..(a^e) = a^(e+..d..+e) = a^(d*e)

Why does decrypt take so many more resources than encrypt?
The size of the keys!
--}

main :: IO ()
main = do  
  !f <- BL.readFile "./app/bigger_text.txt"
  let msg = (map fromIntegral $ BL.unpack f) :: [Integer] -- padding scheme
  let demsg = decrypt.encrypt $ U.fromList msg
  putStrLn $ U.toList $ U.map (chr.fromIntegral) demsg
