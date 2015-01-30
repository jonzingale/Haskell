module CombProb where

import System.Random
import Data.List
import SortsShuffles
import Typehandling 
		--thrufloat

type Z = Integer
type Lattice = [[Z]]
--combinatorials (Its own module perhaps)
fact n = foldr (*) 1 [1..n]
fact2 n r = foldr (*) 1 [ n-k+1 | k<-[1..r] ]
choose n k = (fact2 n k) `div` (fact k)
pascals n = [choose n k|k<-[0..n]]
-------

----Partitions
data P n = P [[Integer]]

parts :: Z -> [Z] -> [Lattice] --all the ways to partition [Z]  into Z parts
parts 0 [] = [[]]              --A jump towards answer to the Parentheses problem!!!-spivak
parts 0 (x:xs) = []            --The length of each list in the list gives Pascals Triangle!
parts _ [] = []
parts n (x:xs) = map (new x) (parts (n-1) xs) ++ map (glue x) (parts n xs)

new :: a -> [[a]] -> [[a]]
new x yss = [x]:yss

glue :: a->[[a]]->[[a]]
glue x (ys:yss) = (x:ys) : yss



-- IO ()
printit [] = putStrLn ""
printit (x:xs) = do (putStrLn.show) x ;
					 printit xs



-- helpers
listToZ :: [Z]->Z
listToZ [] = 0
listToZ (x:xs) = x * 10^(length xs) + listToZ xs
zTolist z = (reverse.f) z
  where 
  		f 0 = []
  		f z = z`mod`10:f (z`div`10) 
		  

		  