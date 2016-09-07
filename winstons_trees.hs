import Data.List
import Control.Applicative
 
data Btree a = Leaf a | Fork (Btree a) (Btree a) 
             deriving (Show , Eq)
 
trees [] = []
trees [x] = [Leaf x]
trees xs = do
    (branchL, branchR) <- splits xs
    liftA2 Fork (trees branchL) $ trees branchR

splits xs = filter noEmpty $ map aux (powerset xs)
    where aux = \ set -> (set, xs \\ set)
          noEmpty = \ (p, p') -> p /= [] && p' /= []
 
powerset ::[a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = xs' ++ map (x:) xs'
    where xs' = powerset xs

splits' (x:xs) = let pow = powerset xs in
								 let comb = \ x -> (tail.zip x) in
			  	 			 let row = (reverse.map (x:)) pow in
			  	 			 comb pow row ++ comb row pow

-- cleaner but slower.
--splits' xs =
--	let it = powerset xs in
--	let them = zip it (reverse it) in
--	(tail.reverse.tail) them