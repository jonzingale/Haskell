module InfiniteList where
import Text.Printf
import Sortable
import Listable

data InfList a = Z {left :: [a], focus :: [a], right :: [a]} deriving (Eq, Ord)

instance Show a => Show (InfList a) where
   show (Z a b c) = printf format (ff reverse a) (ff id b) (ff id c)
    where
      format = "[..%s { %s } %s..]\n"
      ff f = unwords.(map show).f.(take 10)

shiftLeft :: InfList a -> InfList a
shiftLeft (Z (a:as) b cs) = Z as [a] (b++cs)

shiftRight :: InfList a -> InfList a
shiftRight (Z as b (c:cs)) = Z (b++as) [c] cs

integers, ten, randos :: InfList Integer
integers = Z (map negate [1..]) [0] [1..]
ten = limit 10 integers
randos = Z [6,2,4,1] [7] [8,5,3]

limit :: Int -> InfList a -> InfList a
limit n (Z a b c) = Z (take n a) b (take n c)

instance Functor InfList where
  fmap f (Z a b c ) = Z (map f a) (map f b) (map f c)

instance Applicative InfList where
  pure x = Z (repeat x) [x] (repeat x)
  (<*>) (Z fs g hs) (Z as b cs) = Z (fs <*> as) (g <*> b) (hs <*> cs)

instance Foldable InfList where -- ‘foldMap’?
  foldr f base infList = foldr f base $ folds infList 0
    where
      folds (Z xs x []) _ = xs ++ x
      folds (Z [] x xs) _ = x ++ xs 
      folds (Z as b (c:cs)) 0 = b ++ folds (Z as [c] cs) 1
      folds (Z (a:as) b cs) 1 = b ++ folds (Z as [a] cs) 0

instance (Ord a, Eq a) => Listable (InfList a) where
  takeL 0 (Z a b c) = unit
  takeL n (Z a b c) = Z (takeL (n-1) a) b (takeL (n-1) c)
  dropL n (Z [] b []) = unit
  dropL n (Z a b c) = Z (dropL n a) b (dropL n c)
  (+++) zs (Z a b c) = Z ((toList zs) ++ a) b c
  -- (+++) (Z a b c) zs = Z a b (c ++ toList zs)
    where
      toList (Z a b c) = a++b++c
  tailL (Z a b c) = Z a [] c
  cons (Z _ b _) ws = (Z [] b []) +++ ws
  unit = Z [] [] []

instance Ord a => Sortable (InfList a) where -- shuffles are wrong, check cons.
  qsort (Z a b c) = let n = lengthL a in
                    let sort = qsort $ a++b++c in
      Z ((reverse.takeL n) sort) (sort!!!n) (dropL (n+1) sort)


{--
Todo:
Listable
Sortable
Traversable
Monad
--}