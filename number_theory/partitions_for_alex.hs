import Data.List

type Z = Int
type Lattice = [[Z]]

partitions :: Int -> [[Z]]
partitions n = let ary = take n $ repeat 1 in
  pp ary (length ary) []
  where
    pp as 0 accum = unique $ map qsort $ accum
    pp as n accum = pp as (n-1) $ map (map sum) (parts n as) ++ accum

parts :: Z -> [Z] -> [Lattice]
parts 0 [] = [[]]
parts 0 (x:xs) = []
parts _ [] = []
parts n (x:xs) = map (new x) (parts (n-1) xs) ++ map (glue x) (parts n xs)

new :: a -> [[a]] -> [[a]]
new x yss = [x]:yss

glue :: a->[[a]]->[[a]]
glue x (ys:yss) = (x:ys) : yss

unique :: Eq a => [a] -> [a]
unique = reverse . nub . reverse

qsort :: Ord a => [a]->[a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
           where
             smaller = [s | s<-xs, s <= x]
             larger  = [l | l<-xs, l > x]
