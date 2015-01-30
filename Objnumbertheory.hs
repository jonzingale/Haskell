module ObjNumberTheory where 
type Z = Integer
type Domain = Integer
type Codomain = Integer
type Lattice = [[Z]]
type Base = Integer
type Big = Z
type Small = Z

alis :: Z
alis = 8231977

jons :: Z
jons = 5101980

--Either this is broken or expressing something wonderful about Duality!
sVSr :: Big -> Small -> (Z,Z)
sVSr b s = ((totSections b s),(totRetractions s b))

sVSrtup :: (Big,Small)->(Z,Z)
sVSrtup (b,s) = ((totSections b s),(totRetractions s b))
{--That these numbers turn out the same might be made more clear by investigating
one of the methods more fully. I suggest monics and retractions. Here we compute by:
fact d * comb c d to get all of the ways to be monic
mulitplied by d^(c-d) for all of the ways to map all those that dont need to get home.
But these methods don't actually imply digraphing, 
they can be considered 2-way correspondences. 
This observation seems to lie at the heart of understanding
why sVSr should be equal in both components! --}

--make a program to count everybody in a countable infinite list of pairs

{------------MONICS & RETRACTIONS-----------------------
Monos counts all of the monic maps a Domain in  Set to a Codomain in Set.
The idea uses Combinations and Permutations.
  1: Domain choose Codomain give the number of ways of finding in a Codomain
     subsets of size Domain.
  2: Factorial(Domain) gives all the ways a map can give a monic on a give subset.--}

monos :: Domain -> Codomain -> Z
monos d c = if d<=c then fact d * comb c d else 0

        {-- As for Retractions r for a monic m,  
            D -m-> C -r-> D , where rm=id(D) 
            since each element of C pointed at by D must go back to where it
            came from, the remaining elements of C are what have choices.--}                
retractions :: Domain -> Codomain -> Z
retractions d c = if d <= c then d^(c-d) else 0
                             --Note: we do not need to know HOW a map maps.

--Total number of rectractions given every monic from a Domain to a Codomain
totRetractions :: Domain -> Codomain -> Z
totRetractions d c = (monos d c) * (retractions d c)
                            
{-- ----------EPICS & SECTIONS-------------------------
   Epis counts all of the epic maps from a Domain in Set to a Codomain in Set.
   The idea uses the multiplication and inclusion-exclusion principles.
   1: Count the number of ways each element
      of the domain can map to each of the codomain.
   2: We want only Epic maps, so remove all of the ways each element of
      the domain can go to a subset of the codomain short an element. There are
      a numbser of ways to choose which element to leave out of our subset so
      we multiply by the number of ways to pick such a subset (remember combinations
      find the number of subsets a set has of a specific size).
   3: Of these mappings into subsets, not every map covers the subset. Some maps
      into some subsets are counted more than once. 3 -> inclusions(1 -> 2 -> 3).
      Think pointstogirl:{Ali,Jon,Alex} -> {Ali,Jon,Alex}. The subsets {Jon,Ali} and
      {Ali,Alex} will look the same: --}
{--
      {Ali,Jon,Alex}-> Inclusions({Ali}->{Ali,Jon}->{Ali,Jon,Alex})   
      {Ali,Jon,Alex}-> Inclusions({Ali}->{Ali,Alex}->{Ali,Jon,Alex})   
      so we took too many out and we need to add them back in.
      Ultimately we have: listSum [ (add/sub) * #(subsets) * #(maps) ] --}

epis :: Domain -> Codomain -> Z
epis d c = listSum [((-1)^k * (comb c (c-k)) * (c-k)^d) | k <- [0..c] ] 

episList :: Domain -> [(Z,Z)]   -- ( Codomain,#(Epics) ) 
episList d = [(c,epis d c) | c<-[0..d] ]

maxPr2 :: [(Z,Z)] -> [Z] --returns the second component with largest Z value.
maxPr2 [] = []
maxPr2 (x:xs) = [maximum (snd x:maxPr2 xs)]

maxEpic :: Domain -> [Z]  --Returns the largest #(Epics) from a given Domain
maxEpic d =(maxPr2.episList) d

{--
Here begins an epic programming run to represent epimorphisms and to count their sections!
Epimorphisms are represented here as lists where each place value represents an element
of the Codomain and each entry is the number of elements in the Domain referencing it.
For example: an epimorphism hasDated: [Jason,Jon,Shreyas,Brian]->[Ali, Angie] would be
represented as [3,1] where Jason, Jon, and Shreyas point at Ali and Brian to Angie.
generating all possible representations for a given Domain-Codomain pair was a bitch!

A section s for a given Epic e must satisfy: C-s->D-e->C, where es=id(C) .
looking at the invIm(e) we find a discrete stalk for each point p of e's codomain.
These are different possible methods for p to "make it home" es(p) = p. So the
multiplication principle suggests that we multiply together the size of each stalk.
In our example above, the morpie s might be 
   isCurrentlydating: [Ali,Angie]->[Jason,Jon,Shreyas,Brian] where Ali points at Jon
   and Angie points at Brian. We can see that composing these two maps takes Jon back
   to himself and Brian also back to himself. But this is just one of 3 possible sections
   for our original epie hasDated.

See epics, sections', and totSections below . Epis and sVSr above.
--}


         --Parts is a bizarre function----
parts :: Z -> [Z] -> [Lattice] --all the ways to partition [Z]  into Z parts
parts 0 [] = [[]]              --A jump towards answer to the Parentheses problem!!!-spivak
parts 0 (x:xs) = []            --The length of each list in the list gives Pascals Triangle!
parts _ [] = []
parts n (x:xs) = map (new x) (parts (n-1) xs) ++ map (glue x) (parts n xs)

new :: a -> [[a]] -> [[a]]
new x yss = [x]:yss

glue :: a->[[a]]->[[a]]
glue x (ys:yss) = (x:ys) : yss
          ---------------------------------

(!!!) :: [a]->Z->a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)
                          
remdups :: Eq a => [a]->[a]  --The function I have wanting all this time!
remdups [] = []               -- This can make lists into sets
remdups (x:xs) = x:remdups (filter (/= x) xs)

qsort :: Ord a => [a]->[a]  --This even works over sets of sets!
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
           where
             smaller = [s | s<-xs, s<=x]
             larger  = [l | l<-xs, l > x]

qsort' :: Ord a => [a] -> [a]  -- removes duplicates. Why?
qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
           where
             smaller = [s | s<-xs, s<x]
             larger  = [l | l<-xs, l > x]


--necessary for winther
mclose :: Z -> [Lattice]  --mclose 3 = [[[1,1,1]],[[1],[1,1]],[[1],[1],[1]]]
mclose n =  remdups [qsort(latte n!!!l!!!k) |l<-[0..(n-1)],
                                             k<-[0..(size((latte n)!!!l))-1]]
-- *note: k depends on l so def(l) needs to syntactically happen first.

--necessary for winther
listMum :: Lattice->[Z] --modified listSum. listmum (mclose 3!!!1) = [1,2]
listMum []= []
listMum  (x:xs)  = listSum x:listMum xs

--necessary for mclose
latte :: Z->[[Lattice]]    --latte 2 = [[[[1,1]]],[[[1],[1]]]]
latte n = [parts k (ones n) | k<-[1..n] ]

--necessary for epics
winther :: Z -> Lattice --winther 4 = [[4],[1,3],[2,2],[1,1,2],[1,1,1,1]]
winther n = [ listMum(mclose n!!!i)| i<-[0..size(mclose n)-1]]

--A list of the ways a Domain may map onto a Codomain Surjectively in Set.
--necessary for typesandepis , 
epics :: Domain -> Codomain -> Lattice --epics 4 2 = [ [1,3] , [2,2] ]
epics d c = [ winther d!!!j | j<-[0..size(winther d)-1] , size(winther d!!!j) == c]

--necessary for perList and wonderfully modular
timesList :: Eq a => [a] -> [Z] --timesList [3,1,1,2,3] = [2,2,1]
timesList [] = []
timesList (x:xs) = [(size.filter (==x)) (x:xs)]++timesList(filter (/=x) xs) 

--necessary for episoftype
perList :: Eq a => [a] -> Z        --Ways to write an unordered list
perList xs = fact (size xs) `div` (product.map fact) (timesList xs)

--necessary for episoftype
tranny :: [Z] -> [Z]
tranny [] = []
tranny (x:xs) = (comb (sum(x:xs)) x): tranny xs

--necessary for typesandepis , episofeachtype
episoftype :: [Z] -> Z
episoftype xs = (product.tranny)xs * perList xs

--Gives back a list of types(epis) and #.epis(type)
typesandepis :: Domain -> Codomain -> [ ([Z],Z) ]
typesandepis d c = zip (epics d c) (testes d c)
             where testes d c = [episoftype xs | xs<-(epics d c)]


------------------------
epiwhore :: Domain -> Codomain -> [ ([Z],Z) ]
epiwhore d c = zip ((remdups.whorsair.boner d) c) (testes d c)
             where testes d c = [episoftype xs | xs<-((remdups.whorsair.boner d) c)  ]

boner :: Domain->Codomain->[Z]
boner d c = (d-(c-1)):ones (c-1)

cavort :: [a]->a->Int->[a]  --cavort [1,2,3] 5 1 = [1,5,3]
cavort xs a n = ( (take n xs)++[a]++(drop (n+1) xs) )

cohort :: Int->Int->[Z]-> [Z] --Nutrition function
cohort n m xs 
  | n==m = xs
  | otherwise = cavort (cavort xs (xs!!m + 1) m) (xs!!n - 1) n

whorsair :: [Z] -> [[Z]]
whorsair xs = xs: c xs [(n,m)|m<-[0..(length xs-1)],n<-[0..m], n/=m]
 where
  c ys (z:zs)
   | zs == [] = []
   | (cohort (fst z) (snd z) ys) /= (reverse.qsort) (cohort (fst z)(snd z) ys)= c ys zs 
   | otherwise = cohort (fst z) (snd z) ys :whorsair (cohort (fst z) (snd z) ys)++  c ys zs

------------------------------






--necessary for zipesandss
episofeachtype :: Domain -> Codomain -> [Z]
episofeachtype d c = [episoftype xs | xs<-(epics d c)]

--necessary for zipesandss
sections' :: Lattice -> [Z]  -- [ #(sections) ] or (pr2.sections)
sections' xs = [product x| x<-xs]

--necessary for typesandsections , totSections
zipesandss :: Domain->Codomain->[(Z,Z)]
zipesandss d c = zip (episofeachtype d c) (sections'(epics d c))

--necessary for typesandsections , totSections
tuplemult :: [(Z,Z)]->[Z]
tuplemult [] = []
tuplemult (x:xs) = (fst x*(snd x)):tuplemult xs

--Give typesandsections 6 3 = [([1,1,4],360),([1,2,3],2160),([2,2,2],720)]
typesandsections :: Domain->Codomain->[([Z],Z)]
typesandsections d c = zip (epics d c) (tuplemult(zipesandss d c))

--Total#(sections) for all possible epics from a Dom to a Cod.
totSections :: Domain-> Codomain-> Z
totSections d c = sum(tuplemult(zipesandss d c))


{--	SOMETHING IS AFOOT IN SECTIONS LAND!
There is something foul here in ObjNumberTheory Land.
There maybe totSections number of compositions from C->C
via a domain D, but many of those C->D are the same.
That maps need to be divided out of totSections is made
most evident by checking a probability:

Given an epi f:D->C and an arbitrary map s:C->D,
what are the chances that s is a section for f?

answer (if totSections were accurate): re((\d c->(totSections d c,d^c)) 6 4)
ahh but this gives a probability greater than 1!

maybe more accurate would be to notice that a section must be
monic back onto the Set D and this would mean that (d`comb`c)*fact c
would give the total number of monics possible. This divided by the
total maps seems more accurate a choice. Still some thought is needed.
--}
re :: (Z,Z) -> (Z,Z) --reduces two fractions.
re (a,b) = (a `div` (gcd a b), b `div` (gcd a b))

------------------SETS-----------------------
--maps are defined as lists of pairs. a set is pairs from the diagonal, ie an identity map.
--keep 0 free for morphisms like [(0,a),(1,b),(0,c)], denoting 1->3 namely pointing at b.
--pr1 of an element (,) from a mapping denotes the dom. element. pr2 names the codom. element

mSet :: Z -> [(Z,Z)]  --Defines a set of order Z as [(1,1) , (2,2),...,(Z,Z)]
mSet n = [ (k,k) | k<-[1..n] ]

map' :: (a->b) -> [a] -> [b]
map' f xs = [ f x | x<- xs]


-------------Yet to find a home or so very modular

listSum :: [Z] -> Z
listSum [] = 0
listSum (x:xs) = x + listSum xs

comb :: Z -> Z -> Z
comb n k = div (fact n) (fact k * fact (n-k))

fact :: Z -> Z
fact 0 = 1
fact n = n * fact (n-1) 

interseclist :: Eq a => [a] -> [a] -> [a]
interseclist (x:xs) ys = [l | l<- xs , elem l ys ]

baseList :: Z -> Base -> [Z] --baseList 4 2 = [0,0,1]
baseList 0 _ = []
baseList _ 0 = []
baseList n 1 = ones n
baseList n b = mod n b:(baseList.div n) b b 

binar :: Z -> [Z] --baseList 5 2 = [1,0,1]
binar 0 = []
binar n = (mod n 2):binar(div n 2)

countem :: [[Z]] -> Z -> [[Z]] --Selects which elements listSum to a given Integer.
countem xs n = [ l |  l <-xs, listSum l == n ]

ones :: Z -> [Z]     --ones 4 = [1,1,1,1]
ones 0 = []
ones n = 1 : (ones (n-1)) 


partition :: (a -> Bool) -> [a] -> [[a]]
partition p xs = [filter p xs] ++ [filter (not.p) xs]

partition' :: (a -> Bool) -> [a] -> ([a],[a])
partition' p xs = (filter p xs,filter (not.p) xs)

--foldr :: (a->b->b)->b->[a]->b
--foldr f e [] = e
--foldr f e (x:xs) = f x(foldr f e xs)

size :: [a]->Z  --size [[1,1]] = 1
size = foldr oneplus 0
       where oneplus x n=n+1

size' :: [a] -> Z--for comparison with foldr version
size' [] = 0
size' (x:xs) = 1+size' xs

          ------pascals------

pasci :: Z -> [Z] --gives the size of each element of latte. .Note: is n choose k!!!
pasci n = [size ((latte n)!!!k) | k<-[0..(n-1)] ]

pascals :: Z -> [[Z]]--makes pasci's resemblance more obvious.
pascals n = [pasci k | k<-[1..n]]


stirling :: Z->Z->Z
stirling d c = sum[(-1)^k*(comb c k)*(c-k)^d | k<-[0..c]]`div`(fact c)
	where comb n k = div (fact n) (fact k * fact (n-k))

