{-# OPTIONS_GHC -Wno-missing-methods #-} -- for Num AdditiveAbelian

module Abelian where
import System.Random
import Text.Printf
import Data.Char

alphaAction :: AdditiveAbelian -> Char
alphaAction abf = focus.compose abf $ alphabet 

testAlphaAction :: Char
testAlphaAction = alphaAction (shortRandomWalk + shortRandomWalk)

integers :: Zipper Integer
integers = Z (map negate [1..]) 0 [1..]

alphabet :: Zipper Char
alphabet = Z sahpla 'a' (tail alphas)
  where
    alphas = [chr $ mod n 26 + 97  | n<- [0..]]
    sahpla = [chr $ 122 - mod n 26 | n<- [0..]]

{-- 
Zipper Code
--}

data Zipper a = Z {left :: [a], focus :: a, right :: [a]} deriving (Eq, Ord)

shiftLeft :: Zipper a -> Zipper a
shiftLeft (Z (a:as) b cs) = Z as a (b:cs)

shiftRight :: Zipper a -> Zipper a
shiftRight (Z as b (c:cs)) = Z (b:as) c cs

instance Show a => Show (Zipper a) where
   show (Z a b c) = printf format (ff reverse a) (show b) (ff id c)
    where
      format = "[..%s { %s } %s..]\n"
      ff f = unwords.(map show).f.(take 100)

instance Functor Zipper where
  fmap f (Z a b c ) = Z (map f a) (f b) (map f c)

instance Applicative Zipper where
  pure x = Z (repeat x) x (repeat x)
  (<*>) (Z fs g hs) (Z as b cs) = Z (fs <*> as) (g b) (hs <*> cs)

{-- 
Abelian Code
--}

-- All Int positive
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
  compose :: AdditiveAbelian -> v a -> v a
  eval :: Abelian -> v a -> v a

instance Action Zipper where
  compose (Add abs) = eval (foldr mappend mempty abs)
  eval (P n) = (!! n).iterate shiftRight
  eval (N n) = (!! n).iterate shiftLeft

{--
Additive Abelian Code
--}

shortRandomWalk :: AdditiveAbelian
shortRandomWalk = Add $ take (2^15) $ run.(randomRs (-10, 10)).mkStdGen $ 32
  where
    run (x:xs) | x >= 0 = P x : run xs
               | otherwise = N (abs x) : run xs

newtype AdditiveAbelian = Add [Abelian]
additive = Add [P 1, P 2, N 3]

instance Monoid AdditiveAbelian where
  mappend (Add abG) (Add abH) = Add [mappend g h | (g,h) <- zip abG abH]
  mempty = Add [P 0]

instance Num AdditiveAbelian where
  (+) abG abH = abG `mappend` abH