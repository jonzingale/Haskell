module Comonad where
import BaseType

blink :: Comonad w => (w a -> b) -> w a -> w b
blink rule board = board =>> rule

{-- 1D --}
data U x = U [x] x [x]

instance (Monoid a, Eq a, Show a) => Show (U a) where
  show (U a b c) = show $ (reverse.f) a ++ [b] ++ f c
    where f x = take 10 x

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

{-- 2D --}
data V x = V [U x] (U x) [U x]

instance (Monoid a, Eq a, Show a) => Show (V a) where
  show (V as b cs) = unlines.map show $ (reverse.f) as ++ [b] ++ f cs
    where f x = take 10 x

instance Functor V where
  fmap f (V a b c) = V (map (fmap f) a) (fmap f b) (map (fmap f) c)

instance Zipper V where
  left  (V a b c) = V (map left a) (left b) (map left c)
  right (V a b c) = V (map right a) (right b) (map right c)

instance Comonad V where
  coreturn (V _ b _) = coreturn b
  cojoin v =
    let u = U (tiV left v) v (tiV right v) in
    V (tiU (fmap up) u) u (tiU (fmap dn) u)
    where
      tiV f = tail.iterate f
      tiU f = tail.iterate f

instance Connection V where
  up (V a b (c:cs)) = V (b:a) c cs
  dn (V (a:as) b c) = V as a (b:c)

class Connection z where
  up :: z a -> z a
  dn :: z a -> z a