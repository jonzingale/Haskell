
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Abelian where
import Text.Printf
import System.Random

integers :: Zipper Integer
integers = Z (map negate [1..]) 0 [1..]

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
randomWalk :: [Abelian]
randomWalk = run.(randomRs (-10, 10)).mkStdGen $ 32
  where
    run (x:xs) | x >= 0 = P x : run xs
               | otherwise = N (abs x) : run xs

-- both types are intended to be positive Int
data Abelian = P Int | N Int
instance Show Abelian where
  show (N m) = show.negate $ m
  show (P m) = show m

instance Eq Abelian where
  (==) (P n) (N m) = (n==m) && (n==0)
  (==) (P n) (P m) = n == m
  (==) (N n) (N m) = n == m

instance Monoid Abelian where
  mappend (P n) (P m) = P $ n + m
  mappend (P n) (N m) | n - m >= 0 = P $ n - m
                      | otherwise = N $ n - m
  mappend (N n) (P m) | m - n >= 0 = P $ m - n
                      | otherwise = N $ m - n
  mappend (N n) (N m) = P $ n + m
  mempty = P 0

class Action v where -- actions: Ab x G -> G
  eval :: Abelian -> v -> v
  compose :: [Abelian] -> v -> v

instance Action (Zipper v) where
  eval (P n) zipper = (iterate shiftRight zipper)!!n
  eval (N n) zipper = (iterate shiftLeft zipper)!!n
  compose abs zipper = let val = (foldr mappend mempty abs) in eval val zipper


