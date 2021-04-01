module LexTolBwt (lex_tol_bwt) where
import BurrowsWheeler (inv_bwt, bwt)
import Sort (shuffle)

{--
Notes:
- Specifiable lexicographical depth and optimized sort.
- Better time and space savings with nearly the same properties.
- Great performance with only a depth of 4, len 5000 => (0.67 s, 10^9 b)
- Depth of 4 succeeds, depth of 3 fails
- Approximately 9 for many actual English tomes

Questions:
- How can one measure necessary depth?
- Must I construct the rotations? Should I calculate of indices alone?
--}

test0 = lex_tol_bwt 4 tome
test1 i = tome ++ "|" == (inv_bwt.lex_tol_bwt i) tome
test2 i = tome2 ++ "|" == (inv_bwt.lex_tol_bwt i) tome2

tome = take 300 $ foldr (++) "" $ repeat "banana_bandana"
tome2 = "Of course true to form in such dreams the more important a willful act"

-- construct indexed list of rotated and truncated tomes, apply indices to
-- original tome. 
lex_tol_bwt :: Int -> String -> String
lex_tol_bwt i xs =
  let xss = '|' : xs in
  let len = length xss in
  let idxsLs = zip (truncRotate i xss) [0..] in
  let idxs = rsort.take len $ idxsLs in
  -- grabs the last char in the cycle by translation of index
  map (\i -> xss!!(mod (i-1) len)) idxs

truncRotate :: Int -> String -> [String]
truncRotate i (x:xs) = take i (x:xs) : truncRotate i (xs ++ [x])

-- modified qsort with randomized input, returns indexes for tome
rsort :: [(String, Int)] -> [Int]
rsort = sort.shuffle
  where
    sort [] = []
    sort [(s,i)] = [i]
    sort ((s,i):xs) = less (s,i) xs ++ [i] ++ more (s,i) xs
    less a bs = sort $ filter (<= a) bs
    more a bs = sort $ filter (> a) bs
