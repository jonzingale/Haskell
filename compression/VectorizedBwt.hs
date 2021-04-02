module VectorizedBwt where
import qualified Data.Vector.Unboxed as V
import Sort (shuffle)

{--
Note:
- An attempt to unpack the details of an optimized Burrow-Wheeler transform.
- So far no more time efficient than UnpackedBwt, worse on space.
--}

test = burrows_wheeler tome
tome = foldr (++) "" $ repeat "banana_bandana" -- infinite

-- efficiently generate tome of length 1024.
sized_tome :: String -> V.Vector Char
sized_tome text = V.cons '|' $ V.generate 1024 (\j -> text !! j)

-- Note: chunked into blocks of size 1024
burrows_wheeler :: String -> String
burrows_wheeler str =
  let st = sized_tome str in
  sort.shuffle.(take (V.length st)).rotate $ st
  where
    rotate vect = vect : rotate (V.snoc (V.tail vect) (V.head vect))

-- TODO: Write such that the output as indices rather than strings.
sort :: [V.Vector Char] -> String
sort [] = []
sort [x] = [V.last x]
sort (x:xs) = sort (less x xs) ++ [V.last x] ++ sort (more x xs)
  where
    less a bs = filter (<= a) bs
    more a bs = filter (> a) bs
