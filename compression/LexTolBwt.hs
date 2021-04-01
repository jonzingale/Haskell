module LexTolBwt (lex_tol_bwt) where
import BurrowsWheeler (inv_bwt, bwt)
import Sort (shuffle)

{--
Notes:
- Specifiable lexigraphic depth and optimized sort.
- Better time and space savings with nearly the same properties.
- Great performance with only a depth of 4, len 5000 => (0.67 s, 10^9 b)
- Depth of 4 succeeds, depth of 3 fails
- How can one measure necessary depth?
--}

test1 = lex_tol_bwt 4 tome
test2 i = tome ++ "|" == (inv_bwt.lex_tol_bwt i) tome

tome = take 100 $ foldr (++) "" $ repeat "banana_bandana"

-- construct indexed list of rotated and truncated tomes, apply indices to
-- original tome. 
lex_tol_bwt :: Int -> String -> String
lex_tol_bwt i xs =
  let len = length xs + 1 in
  let xss = '|' : xs in
  let idxsLs = zip (truncRotate i xss) [0..] in
  let idxs = rsort.(take len) $ idxsLs in
  -- grabs the last char in the cycle by translation of index
  map (\i -> xss!!(mod (i-1) len)) idxs

truncRotate :: Int -> String -> [String]
truncRotate i (x:xs) = (take i (x:xs)) : truncRotate i (xs ++ [x])

-- modified qsort with randomized input, returns indexes for tome
rsort :: [(String, Int)] -> [Int]
rsort = sort.shuffle
  where
    sort [] = []
    sort [(s,i)] = [i]
    sort ((s,i):xs) = sort (less (s,i) xs) ++ [i] ++ sort (more (s,i) xs)
    less a bs = filter (<= a) bs
    more a bs = filter (> a) bs
