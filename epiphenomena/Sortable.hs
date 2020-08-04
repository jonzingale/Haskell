module Sortable where
import System.Random

data Pair a b = P a b deriving (Show, Eq)

instance (Ord a, Eq b) => Ord (Pair a b) where
  (<=) x y = pr1 x <= pr1 y
  (>) x y = pr1 x > pr1 y

instance KeySortable Pair where
  rmap f (P x y) = P x (f y) -- really an fmap
  diag x = P x x -- forces explictly typing
  pr1 (P x y) = x
  pr2 (P x y) = y

class KeySortable p where
  sort :: Ord x => [p x y] -> [p x y]
  sort [] = []
  sort (x:xs) = sort (less x xs) ++ [x] ++ sort (more x xs)
   where
    less a bs = [b | b <- bs, pr1 b <= pr1 a]
    more a bs = [b | b <- bs, pr1 b > pr1 a]

  shuffle :: [y] -> [p Int y]
  shuffle = sort.rKeys
    where
      rKeys ys = [twist.rmap (\y -> x) $ diag y | (x, y) <- zip randos ys]
      randos = randoms (mkStdGen 42) :: [Int]

  twist :: p x y -> p y x
  twist p = rmap (\x -> pr1 p) $ diag (pr2 p)

  rmap :: (y -> z) -> p x y -> p x z
  diag :: x -> p x x
  pr1 :: p x y -> x
  pr2 :: p x y -> y

