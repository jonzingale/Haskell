
{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as L
import qualified Data.ByteString.Lex.Fractional as L
import qualified Data.Vector.Unboxed as U
import System.Environment

{--
Vector parsing via:
https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial

optimize at compilation time:
ghc -Odph --make FileToVector.hs

$ time ./FileToVector
--}

main = do
    !s <- L.readFile "./Tests/data.csv"
    print . U.sum . parse $ s

parse :: L.ByteString -> U.Vector Double
parse = U.unfoldr step
  where
     step !s = case L.readDecimal s of
        Nothing       -> Nothing
        Just (!k, !t) -> Just (k, L.tail t)















