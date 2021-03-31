module UnpackedBwt (burrows_wheeler) where
import Sort (shuffle)

{--
Note: An attempt to unpack the details of an optimized Burrow-Wheeler transform.
Todo: write sort with specifiable lexigraphical tolerance.
--}

test = burrows_wheeler tome
tome = take 1000 $ foldr (++) "" $ repeat "banana_bandana"

burrows_wheeler :: String -> String
burrows_wheeler xs = rsort.(take $ length xs+1).rotate $ '|' : xs
  where rotate (x:xs) = (x:xs) : rotate (xs ++ [x])

-- modified qsort with randomized input
rsort :: [String] -> String
rsort = modSort.shuffle
  where
    modSort [] = []
    modSort [x] = [last x]
    modSort (x:xs) = modSort (less x xs) ++ [last x] ++ modSort (more x xs)
    less a bs = filter (<= a) bs
    more a bs = filter (> a) bs
