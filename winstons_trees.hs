import Data.List
import Control.Applicative
 
data Btree a = Leaf a
             | Fork (Btree a) (Btree a) 
             deriving (Show , Eq)
 
trees [] = []
trees [x] = [Leaf x]
trees xs = do
    (branchL, branchR) <- splits xs
    Fork <$> (trees branchL) <*> (trees branchR)

splits xs = filter noEmpty $ map aux (powerset xs)
    where aux = \ set -> (set, xs \\ set)
          noEmpty = \ (p, p') -> p /= [] && p' /= []
 
powerset ::[a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = (map (x:) xs') ++ xs' 
    where xs' = powerset xs

splits' (x:xs) = let first = map (x:) (powerset xs) in
			  	 let second = (reverse.powerset) xs in
			  	 let comb = \ x -> (tail.zip x) in
			  	 comb first second ++ comb second first
