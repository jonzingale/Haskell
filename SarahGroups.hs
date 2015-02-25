module SarahGroups where
import Data.List
import Data.Tuple

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

type Dyn = [(Int,Int)]
type Target = Int
--type Source = Int
dyn_n :: [Target] -> Dyn
dyn_n t = [(a,b)|(a,b)<-zip [0..(length t)-1] t]
dyn1 = dyn_n [0,0,0,0,1,2,2,3,3,3]

--dyn_TT :: Dyn -> Dyn -> [((Integer,Integer),(Integer,Integer))]
dyn_TT as bs = [((a,b),(s,t))|(a,s)<-as,(b,t)<-bs]

sqr dyn = cl [(n,f b)|((a,b),n)<-zip (d_sqr dyn) [0..length (d_sqr dyn)-1] ]
			where
				f b = head $ findIndices (== b) $ (fst.unzip.d_key.d_sqr) dyn
				d_key dyn = [a|a<-zip ((fst.unzip)dyn) [0..length dyn-1]]
				d_sqr x = dyn_TT x x
				cl = ((map swap).sort.(map swap)) -- orders by target, could be better

-- try to cut down on the swaps
--sqr dyn = cl [(f b,n)|((a,b),n)<-zip (d_sqr dyn) [0..length (d_sqr dyn)-1] ]
--			where
--				f b = head $ findIndices (== b) $ (fst.unzip.d_key.d_sqr) dyn
--				d_key dyn = [a|a<-zip ((fst.unzip)dyn) [0..length dyn-1]]
--				d_sqr x = dyn_TT x x
--				cl = (map swap).sort -- orders by target, could be better


-- from (sqr) dyn
clean_dyn [] = [] -- [ (target,[sources]),#sources ]
clean_dyn ((s,t):dyn) = let it = partition ((== t).snd) ((s,t):dyn) in
						(t,(fst.unzip.fst) it,(length.fst)it ) : (clean_dyn .dropWhile((== t).snd)) dyn

pretty_dyn dyn = putStr $foldr ((++).(++ "\n").show) "\n"  $ (clean_dyn.sqr) dyn



