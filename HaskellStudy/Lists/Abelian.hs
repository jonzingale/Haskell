module Abelian where
import Text.Printf
-- import Sortable
-- import Listable

data Zipper a = Z {left :: [a], focus :: a, right :: [a]} deriving (Eq, Ord)

instance Show a => Show (Zipper a) where
   show (Z a b c) = printf format (ff reverse a) (show b) (ff id c)
    where
      format = "[..%s { %s } %s..]\n"
      ff f = unwords.(map show).f.(take 8)

shiftLeft :: Zipper a -> Zipper a
shiftLeft (Z (a:as) b cs) = Z as a (b:cs)

shiftRight :: Zipper a -> Zipper a
shiftRight (Z as b (c:cs)) = Z (b:as) c cs

integers, ten, randos :: Zipper Integer
integers = Z (map negate [1..]) 0 [1..]
ten = limit 10 integers
randos = Z [6,2,4,1] 7 [8,5,3]

limit :: Int -> Zipper a -> Zipper a
limit n (Z a b c) = Z (take n a) b (take n c)

instance Functor Zipper where
  fmap f (Z a b c ) = Z (map f a) (f b) (map f c)

instance Applicative Zipper where
  pure x = Z (repeat x) x (repeat x)
  (<*>) (Z fs g hs) (Z as b cs) = Z (fs <*> as) (g b) (hs <*> cs)

{-- 
My goal with Abelian is to write an algebraic
interface to Zipper, so that rotations and evaluations
can be performed on pointers and then some minimal
computations to return the actual value.
--}

class Abelian z where
  identity :: z a -> z a
