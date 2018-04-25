
-- https://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array.html
module RayTracer.Lattice where
import RayTracer.Rhythm
import System.Random
import System.IO

-- import qualified Data.Array.Accelerate as A -- GPU
-- import qualified Data.Array.Repa as R -- wrapper for fast
-- import qualified Data.Conduit as C -- for streaming parallel.
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Array.Unboxed as U -- strict fast Arrays
import qualified Data.Array.IArray as I -- fast lookups on Unboxed (I.!)

--import qualified Data.ByteString.Lex.Lazy.Double as L as L
import qualified RayTracer.ReadDouble as R
import qualified Data.Array as A -- mostly for comparison
import Data.List.Split
-- import qualified Data.Vector.Storable as V

-- import Data.ByteString.Internal as BS
-- import qualified Data.Vector.Storable as V
{--
TODO:
Make a 1000 cubic-cell mesh and calculate the path sums through
the mesh with a function at every point. The value of the function
gives the amount of attenuation through the mesh.

Further, the length of the ray through each given
cell is needed to compute the value at the given cell.

10^7 => (12.37 secs, 24,200,154,144 bytes)
10^8 => (117.79 secs, 253,200,261,744 bytes)



10^6 Float => 10MB
10^6 Double => 20MB, compressed 9MB
10^6 SparseDouble (1/20) => 4MB, compressed 78Kb
10^8 SparseDouble (1/20) => 400MB, compressed 7MB ~ 300 secs to save
10^9 SparseDouble (1/40) =>
--}
-- 10^7 like a champ, 10^8 gets challenging.
-- rArray takes seed values to produce random arrays.

sparseArray :: U.UArray Int Double
sparseArray = U.listArray bounds sparse
  where
    sparse = [ if k == 0 then t else 0.0::Double | (k, t) <- zip randos randIntv]
    bounds = (0::Int, 10^9-1)
    randos = randomRs (0, 40::Int).mkStdGen $ 42
    randIntv = randomRs (0, 1::Double).mkStdGen $ 42 

bigArray :: U.UArray Int Double
bigArray = U.listArray bounds $ randomRs (0, 10**3::Double).mkStdGen $ 42
  where bounds = (0::Int, 10^5-1)

exBigA = (I.!) bigArray 343

saveArr = do -- space delimited csv
  writeFile "./Tests/testArray.csv" $ unwords.map show $ U.elems bigArray

-- bytesToFloats :: BS.ByteString -> V.Vector Float
-- bytesToFloats = V.unsafeCast . aux . BS.toForeignPtr
--   where aux (fp,offset,len) = V.unsafeFromForeignPtr fp offset len

-- testRead n = do
  -- content <- L.readFile "./Tests/testBigSparseArray.csv"
  -- return (L.take n$ content)

testRead = do
  content <- L.readFile "./Tests/testArray.csv"
  let ary = (map (read.(L.unpack)) $ L.words content)::[Double]
  return $ sum ary

testRead2 = do -- better would be Array not List
  content <- L.readFile "./Tests/testBigSparseArray.csv"
  let doubles = map R.readDouble (L.words content)
  return $ doubles

-- Check here for Parsing.
-- https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial#Parsing_Binary_Data

-- import Data.ByteString.Internal as BS

-- bytesToFloats :: L.ByteString -> V.Vector Float
-- bytesToFloats = V.unsafeCast . aux . L.toForeignPtr
--   where aux (fp,offset,len) = V.unsafeFromForeignPtr fp offset len








