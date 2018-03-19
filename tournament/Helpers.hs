module Helpers (hhsort, havelhakimi, havelLines, hhAdjacency) where

data Vertex = V { name::String, degree::Int} deriving Eq
data Edge = E { source::Vertex, target::Vertex }
data Graph = G { edges::[Edge] } deriving Show

instance Show Vertex where
  show (V a b) = a
instance Show Edge where
  show (E a b) = show a ++ "->" ++ show b

instance Ord Vertex where
  (<=) (V ss n) (V tt m) = n <= m
  (>=) (V ss n) (V tt m) = n >= m

-------------

havelhakimi :: [Int] -> Bool
havelhakimi (a:[]) = a == 0
havelhakimi (a:as) = havelhakimi.hhsort $
  map (subtract 1) (take a as) ++ drop a as

-- displays the havel hakimi process
havelLines :: [Int] -> String
havelLines list = unwords $ f list [] 1
  where
    f (a:[]) accum i = accum
    f as accum i = f (hhsort.hh $ as) (accum ++ [buildRow as i]) (i+1)
    hh (a:as) = map (subtract 1) (take a as) ++ drop a as
    buildRow as i = show $ zeros i ++ hh as

-- produce an adjacency graph if havelHakimi.
hhAdjacency :: [Int] -> Maybe [Int]
hhAdjacency list | havelhakimi list = Just $ f list [] (length list) 1 list
                 | otherwise = Nothing
  where
    keySort (a:as) = snd.unzip.hhsort.zip as
    hh (a:as) = map (subtract 1) (take a as) ++ drop a as
    buildR a ary n i ls = ary ++ zeros i ++ keySort ls (ones a ++ zeros (n-a-1))

    f [a] accum n i ls = buildR a accum n i ls
    f (a:as) accum n i ls = buildR a accum n i ls ++
                            f (hhsort.hh $ a:as) accum (n-1) (i+1) (hh (a:as))

-- notice that it sorts large to small
hhsort :: Ord a => [a] -> [a]
hhsort [] = []
hhsort (x:xs) = hhsort larger ++ [x] ++ hhsort smaller
           where
             smaller = [s | s<-xs, s<=x]
             larger  = [l | l<-xs, l > x]

zeros n = take n $ repeat 0
ones  n = take n $ repeat 1