module Sort where

ary = "afjqhmbe"

insert :: Ord a => a -> [a] -> [a]
insert x xs = takeWhile (< x) xs ++ [x] ++ dropWhile (< x) xs

sort :: Ord a => [a] -> [a]
sort xs = f xs []
  where
    f [] acc = acc
    f (x:xs) acc = f xs (insert x acc)