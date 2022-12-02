{-# LANGUAGE BangPatterns #-}

module Main where
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as U
import Data.Char

{--
https://www.schoolofhaskell.com/user/tekul/bytestring-bits-and-pieces

c = m^e `mod` m
d = c^(inv e) `mod` m
(a^e)^d == (a^e)(a^e)..d..(a^e) = a^(e+..d..+e) = a^(d*e)

Why does decrypt take so many more resources than encrypt?
The size of the keys!
--}

type Key = Integer
type Modulus = Integer
type Message = U.Vector Integer

main :: IO ()
main = do  
  !f <- BL.readFile "./app/bigger_text.txt"
  let msg = (map fromIntegral $ BL.unpack f) :: [Integer] -- padding scheme
  let demsg = decrypt.encrypt $ U.fromList msg
  putStrLn $ U.toList $ U.map (chr.fromIntegral) demsg

modulus, ekey, dkey :: Integer
ekey = 1823 -- some values work
dkey = inverse modulus ekey
modulus = 1987 -- 104161

encrypt :: Message -> Message
encrypt msg = U.map (\t -> mod (t^ekey) modulus) msg

decrypt :: Message -> Message
decrypt msg = U.map (\t -> mod (t^dkey) modulus) msg

inverse :: Modulus -> Key -> Key
inverse m a = head [b | b <- [1..m], mod (b*a) (m-1) == 1]
