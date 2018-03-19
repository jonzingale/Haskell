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
havelLines list = unwords.(map show).f $ list
  where
    f (a:[]) = [[a]]
    f (a:as) = (a:as) : (f.g) (a:as)
    g (a:as) = hhsort $ map (subtract 1) (take a as) ++ drop a as

-- produce an adjacency graph if havelHakimi
hhAdjacency :: [Int] -> Maybe [Int]
hhAdjacency list | havelhakimi list = Just $ f list (length list) 1
                 | otherwise =  Nothing
  where
    zeros n = take n $ repeat 0
    ones  n = take n $ repeat 1
    f (a:[]) n i = zeros i++ones a++zeros (n-a-1)
    f (a:as) n i = zeros i++ones a++zeros (n-a-1) ++ f (g (a:as)) (n-1) (i+1)
    g (a:as) = hhsort $ map (subtract 1) (take a as) ++ drop a as

-- notice that it sorts large to small
hhsort :: Ord a => [a] -> [a]
hhsort [] = []
hhsort (x:xs) = hhsort larger ++ [x] ++ hhsort smaller
           where
             smaller = [s | s<-xs, s<=x]
             larger  = [l | l<-xs, l > x]