module SarahGroups where
import Data.Function (on)
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
type Source = Int
type Target = Int
sources = fst.unzip
targets = snd.unzip

dyn_n :: [Target] -> Dyn
dyn_n t = [(a,b)|(a,b)<-zip [0..] t]
dyn1 = dyn_n [0,0,0,0,1,2,2,3,3,3]
dyn2 = dyn_n [0,0,0,2,2,1]
dyn3 = dyn_n [0,0,1,2,3]

dTT :: Dyn -> Dyn -> Dyn
dTT cyn dyn = cl [(n,f b)|((a,b),n)<-zip (dyn_TT cyn dyn) [0..] ]
			where
				dyn_TT as bs = [((a,b),(s,t))|(a,s)<-as,(b,t)<-bs]
				f b = head $ findIndices (== b) $ (sources.dyn_TT cyn) dyn
				cl = sortBy (compare `on` (snd))

hop :: Dyn -> Dyn
hop dyn = zip [0..] $ t_hop ((t_drop.targets) dyn) (targets dyn)
	where
		t_hop [] _ = [] -- blink
		t_hop (x:xs) ts = ts!!x : t_hop xs ( ts)
		t_drop (x:xs) = x: dropWhile(==0) xs

--s_hop :: Dyn -> Dyn -- drops ends.

-- TO DISPLAY A DYN IN PARTS
pretty :: Show a => [a] -> IO ()
pretty xs = putStr $foldr ((++).(++ "\n").show) "\n"  $ xs

dynamate dyn = pretty $ (takeWhile (/=[(0,0)])  (iterate hop dyn)) ++ [[(0,0)]]
pretty_dyn dyn = pretty $ clean_dyn dyn
	where
		clean_dyn [] = [] -- [ (target,[sources]),#sources ]
		clean_dyn ((s,t):dyn) = let it = partition ((== t).snd) ((s,t):dyn) in
														let unclean = (clean_dyn.dropWhile((== t).snd)) in
														(t,(sources.fst) it,(length.fst)it) : unclean dyn

