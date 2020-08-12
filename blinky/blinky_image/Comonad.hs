module Comonad where
import BaseType

zs :: U Int
zs = U (map (\x -> -x) $ [1..]) 0 [1..]

blink :: Comonad w => (w a -> b) -> w a -> w b
blink rule board = board =>> rule

data U x = U [x] x [x]

instance (Monoid a, Eq a) => Show (U a) where
  show (U a b c) = show $ (reverse.f.gg $ a) ++ gg [b] ++ (f.gg) c
    where
      gg = map (\a -> if a == mempty then ' ' else '.')
      f x = take 25 x

instance Functor U where
  fmap f (U a b c) = U (map f a) (f b) (map f c)

instance Zipper U where
  right (U a b (c:cs)) = U (b:a) c cs
  left  (U (a:as) b c) = U as a (b:c)

instance Comonad U where
  cojoin a = U (tail.iterate left $ a) a (tail.iterate right $ a)
  coreturn (U _ b _) = b

class Zipper z where
  right :: z a -> z a
  left :: z a -> z a

class Functor w => Comonad w where
  (=>>) :: w a -> (w a -> b) -> w b
  x =>> f = fmap f (cojoin x)
  coreturn :: w a -> a
  cojoin   :: w a -> w (w a)
