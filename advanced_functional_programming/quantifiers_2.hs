-- an attempt to encode the quantifiers

{-- Todo:
  *lift things correctly.
  *understand the dependence on Dom, Cod
  *extend to arbitrary mappings via functor class.

  It would be best to not have to evaluate anything
  until a proper S on P(uxv) is given. This can be
  accomplished for ∀ by noticing that it is all of
  the diagonal elements of P(uxv).

--}
import Control.Monad
import Data.Set hiding (map)
{--
singleton :: a -> Set a
fromList :: Ord a => [a] -> Set a

insert :: Ord a => a -> Set a -> Set a

intersection :: Ord a => Set a -> Set a -> Set a
difference :: Ord a => Set a -> Set a -> Set a
union :: Ord a => Set a -> Set a -> Set a

elems :: Set a -> [a]
empty :: Set a
--}

-- data PairsPart f = S f (V [a]) | Nothing deriving (Show , Eq)
data V = One | Two | Three | Four deriving (Show , Eq)
data Vx_ a = P [a] | Empty deriving (Show , Eq)

vs = [One, Two]

eval (P us) = [(x,y) | x <- us, y <- vs]
eval Empty = []
pr1 (P a) = a
pr1 Empty = []
pr2 (P a) = vs
pr2 Empty = []

-- ⊥⊥⊥
fibers :: [a] -> [Vx_ a]
fibers xs = map P $ map (:[]) xs
p_star = fibers -- [P[1],P[2],P[3]]

-- pairs (P xs ys) = zip xs ys
-- pairs Empty = []

-- pr_star :: Int -> Base_U Int
-- pr_star a | elem a (pr1 example) = P [a] vs
--           | otherwise = Empty

-- -- direct_image
-- exists :: Base_U Int -> [Int]
-- exists Empty = []
-- exists ss | elem (pairs ss) $ (powerbase.pr1) example = pr1 ss
--           | otherwise = []

-- -- glb cylinder in S
-- forall :: Base_U Int -> [Int]
-- forall Empty = []
-- forall ss = map fst $ filter (\(x,y)->y == vs) $ roundup.pairs $ ss
--   where
--     roundup [] = []
--     roundup ss = ff ss : roundup (gg ss)
--     ff ((x,y):tt) = (x, y : ((snd.unzip.takeWhile ((== x).fst)) tt))
--     gg ((x,y):tt) = dropWhile ((== x).fst) tt

-- --Helpers
powerset xs = filterM (\x -> [True, False]) xs
total_space as = [(x,y) | x<-as, y<-vs]
powerbase = powerset.total_space

-- -- Constants and Constant Makers
-- empty_forall = _S 62
-- example = P [0..2] vs

-- _S :: Int -> Base_U Int -- takes a number, returns a subset
-- _S n | n >= (length.powerbase.pr1) example || n < 0 = Empty 
     -- | otherwise = uncurry P (unzip(((powerbase.pr1) example)!!n))
     -- really _S should be related to a data sub-type, S.
     -- I keep thinking a function type that once I hand
     -- some Stateful data type an array, all calculations
     -- are on that UxV.