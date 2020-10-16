module Comonad where
import System.Random
import Data.Char

data U x = U [x] x [x]

instance Show a => Show (U a) where
  show (U a b c) = show (a++[b]++c)

right (U a b (c:cs)) = U (b:a) c cs
left  (U (a:as) b c) = U as a (b:c)

instance Functor U where
  fmap f (U a b c) = U (map f a) (f b) (map f c)

class Functor w => Comonad w where
  (=>>)    :: w a -> (w a -> b) -> w b
  coreturn :: w a -> a
  cojoin   :: w a -> w (w a)
  x =>> f = fmap f (cojoin x)

instance Comonad U where
   cojoin a = U (tail $ iterate left a) a (tail $ iterate right a)
   coreturn (U _ b _) = b
