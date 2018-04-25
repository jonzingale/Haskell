
{-# LANGUAGE BangPatterns #-}
-- module VectorLattice where
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector.Unboxed as V
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

-- testRead2 = do -- better would be Array not List
--   content <- L.readFile "./Tests/testBigSparseArray.csv"
--   return $ V.fromList $ L.words $ content

  -- let doubles = map R.readDouble (L.words content)
  -- return $ doubles

-- testRead = do
-- content <- L.readFile "./Tests/testArray.csv"
-- let ary = (map (read.(L.unpack)) $ L.words content)::[Double]
-- return $ sum ary

-- "./Tests/testArray.csv"

main = do
    -- [f] <- getArgs
    -- s   <- L.readFile f
    !s <- L.readFile "./Tests/data.csv"
    print . V.sum . parse $ s
 
-- Fill a new vector from a file containing a list of numbers.
parse = V.unfoldr step
  where
     step !s = case L.readInt s of
        Nothing       -> Nothing
        Just (!k, !t) -> Just (k, L.tail t)