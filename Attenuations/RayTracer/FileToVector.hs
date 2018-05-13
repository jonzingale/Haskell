
{-# LANGUAGE BangPatterns #-}
module RayTracer.FileToVector (qArray, qAry49, anArray, fortyNineDoubles) where
import qualified Data.ByteString.Lex.Fractional as L
import qualified Data.ByteString.Char8 as L
import qualified Data.Vector.Unboxed as U
import System.Environment

{--
Vector parsing via:
https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial

optimize at compilation time:
$ ghc -Odph --make FileToVector.hs
$ time ./FileToVector
--}

-- The basic idea:
main = do
    !s <- L.readFile "./Tests/data1M" -- 1_000_000
    print . U.sum . parse $ s

parse :: L.ByteString -> U.Vector Double
parse = U.unfoldr step
  where
     step !s = case L.readExponential s of
        Nothing       -> Nothing
        Just (!k, !t) -> Just (k, L.tail t)

-- A run at a useful Parser
anArray = do
  !s <- L.readFile "./Tests/data1M"
  return.parse $ s

fortyNineDoubles = do
  !s <- L.readFile "./Tests/data49Doubles"
  return.parse $ s

type Coords = (Int, Int)
qArray :: U.Unbox a => Coords -> U.Vector a -> a
qArray (x, y) a = (U.!) a (x + y * 1000)

qAry49 :: U.Unbox a => Coords -> U.Vector a -> a
qAry49 (x, y) a = (U.!) a (x + y * 7)

testIndex = do
  ary <- anArray
  return $ qArray (20, 300) ary









