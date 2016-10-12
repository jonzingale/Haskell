-- an attempt to encode the quantifiers

{-- Todo:
  *lift things correctly.
  *understand the dependence on Dom, Cod
  *extend to arbitrary mappings via functor class.

  Works but can use some work.
--}
import Control.Monad

data V = One | Two | Three | Four deriving (Show , Eq)
data Base_U a = P [a] [V] | Empty deriving (Show , Eq)

vs = [One, Two]

pr1 (P a b) = a
pr1 Empty = []
pr2 (P a b) = b
pr2 Empty = []
pairs (P xs ys) = zip xs ys
pairs Empty = []

pr_star :: Int -> Base_U Int
pr_star a | elem a (pr1 example) = P [a] vs
          | otherwise = Empty

-- direct_image
exists :: Base_U Int -> [Int]
exists Empty = []
exists ss | elem (pairs ss) $ (powerbase.pr1) example = pr1 ss
          | otherwise = []

-- glb cylinder in S
forall :: Base_U Int -> [Int]
forall Empty = []
forall ss = map fst $ filter (\(x,y)->y == vs) $ roundup.pairs $ ss
  where
    roundup [] = []
    roundup ss = ff ss : roundup (gg ss)
    ff ((x,y):tt) = (x, y : ((snd.unzip.takeWhile ((== x).fst)) tt))
    gg ((x,y):tt) = dropWhile ((== x).fst) tt

--Helpers
powerset xs = filterM (\x -> [True, False]) xs
productbase as = [(x,y) | x<-as, y<-vs]
powerbase = powerset.productbase

-- Constants and Constant Makers
empty_forall = _S 62
example = P [0..2] vs

_S :: Int -> Base_U Int -- takes a number, returns a subset
_S n | n >= (length.powerbase.pr1) example || n < 0 = Empty 
     | otherwise = uncurry P (unzip(((powerbase.pr1) example)!!n))
     -- really _S should be related to a data sub-type, S.
