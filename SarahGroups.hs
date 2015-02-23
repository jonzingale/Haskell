module SarahGroups where
import Data.List

count :: Integer -> [Integer]
count n = [a|a<-[0..n-1]]

counts_count :: Integer -> Integer -> [(Integer,Integer)]
counts_count n m = [(a,b)|a <- count n, b <- count m]

m_table :: Integer -> [Integer]
m_table n = [(a*b)`mod` n |a <- count n, b <- count n]

alpha = ['a'..'z']
vowels = ['a','e','i','o','u']
consn = alpha \\ vowels

key :: [(Integer,Char)]
key = zip [0..21] consn

decode :: [Integer] -> [Char]
decode [] = []
decode (x:xs) = find_by x key : decode xs

find_by :: Eq a => a -> [(a,b)] -> b
find_by n ns = head[j|(i,j)<-ns, n==i]

split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = take n xs : split n (drop n xs)

n_group n =  ((split n).decode.m_table.toInteger) n
{--
[(0,0),(0,1),(0,2),
(1,0),(1,1),(1,2),
(2,0),(2,1),(2,2),
(3,0),(3,1),(3,2),
(4,0),(4,1),(4,2)]

0,0,0,0,0,
0,1,2,3,4,
0,2,4,1,3,
0,3,1,4,2,
0,4,3,2,1]

0,0,0,0,0,0,
0,1,2,3,4,5,
0,2,4,0,2,4,
0,3,0,3,0,3,
0,4,2,0,4,2,
0,5,4,3,2,1


--}