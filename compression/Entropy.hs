module Entropy where
import Data.List (sort)
{--
Notes:
- From https://www.shannonentropy.netmark.pl/calculate
- Not helpful for determining Kolmogorov complexity,
  entropy xs == (entropy.sort) xs
- Shannon Entropy
- Metric Entropy

Todo:
- Find reasonable estimation of Kolmogorov complexity
- For large tomes, use standard language specific letter frequencies.
--}

text = "of course true to form in such dreams the more important a willful act"
tome = take 5000 $ foldr (++) "" $ repeat text

-- Shannon entropy
entropy :: Eq a => [a] -> Float
entropy [] = 0
entropy (x:xs) =
  let len = fromIntegral.length $ (x:xs) in
  let monomial z = (z/len) * logBase 2 (z/len) in
  negate.sum.map monomial $ f (x, 1) xs []
  where
    -- all lists processed
    f (s, z) [] [] = [z]
    -- all characters of a kind processed
    f (s, z) [] (a:acc) = z : f (a, 1) acc []
    -- process all characters of a kind
    f (s, z) (t:ts) acc
      | t == s = f (s, z+1) ts acc
      | otherwise = f (s, z) ts (t:acc)

metricEntropy :: Eq a => [a] -> Float
metricEntropy str = entropy str / (fromIntegral.length $ str)

-- calculate character counts
counts :: Eq a => [a] -> [(a, Float)]
counts [] = []
counts (x:xs) = f (x, 1) xs []
  where
    -- all lists processed
    f s_i [] [] = [s_i]
    -- all characters of a kind processed
    f s_i [] (a:acc) = s_i : f (a, 1) acc []
    -- process all characters of a kind
    f (s, i) (t:ts) acc
      | t == s = f (s, i+1) ts acc
      | otherwise = f (s, i) ts (t:acc)
