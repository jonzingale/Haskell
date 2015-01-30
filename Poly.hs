module Poly where
{-- Convention: a + bx + cx^2 --}
import Data.List

-- P [1,2,3] = x^2 + 2x + 3 ?
data Poly x = P [x] deriving (Show)

--Functorials
incl x = P x
counit (P x) = x
--eval (P x) n = (sum.(map (* n)).counit) (P x)

eval (P []) n = 0
eval (P [x]) _ = x
eval (P (x:xs)) n = (x * n^(length xs)) + (eval (P xs) n)


--Polynomial operations

-- Many of these functions are not properly tied to the Poly x datatype


polyP xs ys = --mulitplies two polynomials polyP [1,1] [1,2,1] = [1,3,3,1]    OPPS BUGGY polyP [1,1] [1,3,3,1] gives [1,4,6]
	let ss = zip walk xs in
	let tt = zip walk ys in
	let beast = [(i+j,n*m)|(i,n)<-ss , (j,m)<-tt] in
    [ (sum.snd.unzip) it| it<-((bosss 0).sort) beast]
       where
       	bosss n x |  snd (partition ((< n).fst) x) == [] = []
                  | otherwise = (fst (partition ((== n).fst) x)) : (bosss (n+1) (snd (partition ((== n).fst) x)) )

npolyP 0 xs ys = ys  --note: second argument is favored
npolyP n xs ys = npolyP (n-1) xs (polyP  xs ys)

polyFact n = foldr polyP [0,1] [[-i,1] |i<-[1..n]] -- n(n-1)...(n-r+1)

polyE xs ys = --sums two polynomials
 [i+j|(i,j)<-zip xs ys]

del (P (x:[])) = (P [0]) --differentiates a polynomial
del polyx =P $ tail[ x * i | (x,i) <- zip (counit polyx) walk]

ndel 0 polyx = polyx --nth differential
ndel n polyx = ndel (n-1) (del polyx)






--Helpers and Novelties
ones = 0:ones
walk = [0..]
listpolyP n xs ys = take n (iterate (polyP xs) ys) -- mpolyP 3 [1,1] [1,1] = [[1,1],[1,2,1],[1,3,3,1]]
