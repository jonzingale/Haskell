module Graphs where
import Helpers

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

triangle = G [E (V "a" 2) (V "b" 2),
              E (V "b" 2) (V "c" 2),
              E (V "c" 2) (V "a" 2)]

havelhakimi :: [Int] -> Bool
havelhakimi (a:[]) = a == 0
havelhakimi (a:as) = havelhakimi.qsort $
  map (+ (-1)) (take a as) ++ drop a as

havelLines :: [Int] -> String
havelLines list = unwords.(map show).f $ list
  where
    f (a:[]) = [[a]]
    f (a:as) = (a:as) : (f.g) (a:as)
    g (a:as) = qsort $ (map (+ (-1)) (take a as)) ++ drop a as

{--
tournament :: Int -> [Edge]
tournament n | n < 8 || odd n = []
             | otherwise = hh [] [(7,k) | k <- [1..n]]

hh :: [Edge] -> [(Degree, Vertex)] -> [Edge]
hh edge_list [] = edge_list
hh edge_list pairs = let sorted = qsort pairs in
  hh (edge_list ++ edges sorted) $ f sorted
  where
    f ((a,b):as) = qsort $ fst_map (+ (-1)) (take a as) ++ drop a as
    edges ((a,b):as) = [(b, q) | (p,q) <- take a as]
    fst_map f xs =  [(f a, b)  | (a,b) <- xs]
--}


-- Pairs will almost always be edges.
hh :: [Edge] -> [Vertex] -> [Edge]
hh edge_list [] = edge_list
hh edge_list verts = let sorted = qsort verts in
  hh (edge_list ++ edges sorted) $ sorted -- f sorted
  where
    f ((V ss n):as) = qsort $ snd_map (+ (-1)) (take n as) ++ drop n as
    edges ((V ss n):as) = [E (V "a" n) (V "b" q) | (V p q) <- take n as]
    snd_map f xs =  [V a (f b)  | (V a b) <- xs]





