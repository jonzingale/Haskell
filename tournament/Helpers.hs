module Helpers (qsort) where

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

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
           where
             smaller = [s | s<-xs, s<=x]
             larger  = [l | l<-xs, l > x]

havelLines :: [Int] -> String -- displays the havel hakimi process
havelLines list = unwords.(map show).f $ list
  where
    f (a:[]) = [[a]]
    f (a:as) = (a:as) : (f.g) (a:as)
    g (a:as) = qsort $ (map (+ (-1)) (take a as)) ++ drop a as