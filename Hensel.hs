module Hensel (niven1,niven2,hensel_do,hensel)where
import Poly

niven1 = P [1,1,7]
niven2 = P [2,3,1,1,11] --whose baseRoots at 7 are both sing and nonsing (([4],[3,5]),7)


hensel_do polyx = do (putStr.unlines.map (prettyit.prett.show)) 
						[  ( (map(map fsst)) (hensel polyx i) ,p)
						|(i,p)<-zip primes (take 300000 primes),
						length (hensel polyx i) > 2 ]

hensel f m = bRoots f m : lift f (least_prime m) (bRoots f m)

--some language for proper lifting
least_prime m = head [p|p<-primes, mod m p == 0 ]
singular (P x) m r = evalM (del (P x) ) (least_prime m) r == 0
evalM (P x) m n = mod (eval (P x) n) m
rootMs (P x) m = [a|a<-[0..(m-1)],evalM (P x) m a == 0 ]

baseRoots :: Poly Integer -> Integer -> ([Integer], [Integer])
baseRoots polyx m = ([r|r<-rootMs polyx (least_prime m), singular polyx m r],
			[r|r<-rootMs polyx (least_prime m), (not.singular polyx m) r])

-- *Hensel> bRoots niven2 7 = [(3,False,False),(4,True,True),(5,False,True)]
bRoots polyx m = [(r, singular polyx m r, 
	  (evalM polyx ((least_prime m)^2) r) == 0)|
					r<-rootMs polyx (least_prime m)]

-- iterates up the tree
lift f p []= []
lift f p bs= (rm.lift_it f p 1) bs: ( lift f p ((rm.lift_it f p 1) bs) )

-- considers a poly prime height and tree-position and lifts one up from there.
lift_it f p h [] = []
lift_it f p h ((r,sing,lifts):rs) 
	| sing==False && lifts == True = [(r, sing , evalM f (p^(h+2)) r==0)]  : lift_it f p (h+1) rs
	| sing==True && lifts==True = [(rs, sing ,evalM f (p^(h+2)) rs==0)| rs<-above r p h] : []
	| otherwise = lift_it f p (h+1) rs

above r p h = r:(takeWhile (/=r)[r+(p^h)*i`mod`(p^(h+1))|i<-[1..]])

--quick and dirty
primes = let fac n= [a|a<-[1..n],n`mod`a==0] in [p|p<-[2..], (length.fac) p < 3]
rm :: [[a]]->[a]
rm [] = []
rm xs = head xs
fsst (a,b,c) = a

--prettyit string = map (\x -> if x==',' || x=='[' || x==']' || x=='(' || x==')' then ' ' else x) string
prettyit string = map (\x -> if x==',' || x=='[' || x=='(' || x==')' then ' ' else x) string
prett string = map (\x -> if x==']' then '.' else x) string
