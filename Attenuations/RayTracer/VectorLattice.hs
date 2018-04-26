
{-# LANGUAGE BangPatterns #-}
-- module VectorLattice where
import qualified Data.ByteString.Char8 as L
-- import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lex.Fractional as L
import qualified Data.Vector.Unboxed as U
import System.Environment

{--
Vector parsing via:
https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial#Parsing_Binary_Data

optimize at compilation time:
ghc -Odph --make vector.hs
ghc -Odph --make VectorLattice.hs

$ time ./vector data
500000500000
./vector data  0.08s user 0.01s system 98% cpu 0.088 total

--}

main = do
    !s <- L.readFile "./Tests/bigdata.csv"
    print . U.sum . parse $ s

parse :: L.ByteString -> U.Vector Double
parse = U.unfoldr step
  where
     step !s = case L.readDecimal s of
        Nothing       -> Nothing
        Just (!k, !t) -> Just (k, L.tail t)















