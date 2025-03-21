module Sortable (Pair, second, diag, pr2, sort, shuffle) where
import Data.Bifunctor
import System.Random

data Pair a b = P a b deriving (Show, Eq)

instance (Ord a, Eq b) => Ord (Pair a b) where
  (<=) x y = pr1 x <= pr1 y
  (>) x y = pr1 x > pr1 y

instance Bifunctor Pair where
  bimap f g (P a b) = P (f a) (g b)
  first f (P a b) = P (f a) b
  second f (P a b) = P a (f b)

instance KeySortable Pair where
  diag x = P x x
  pr1 (P x _) = x
  pr2 (P _ y) = y

class Bifunctor p => KeySortable p where
  sort :: Ord x => [p x y] -> [p x y]
  sort [] = []
  sort (x:xs) = sort (less x xs) ++ [x] ++ sort (more x xs)
   where
    less a bs = [b | b <- bs, pr1 b <= pr1 a]
    more a bs = [b | b <- bs, pr1 b > pr1 a]

  shuffle :: [y] -> [p Int y]
  shuffle = sort.rKeys
    where
      rKeys ys = [first (\y -> x) $ diag y | (x, y) <- zip randos ys]
      randos = randoms (mkStdGen 42) :: [Int]

  diag :: x -> p x x
  pr1 :: p x y -> x
  pr2 :: p x y -> y
