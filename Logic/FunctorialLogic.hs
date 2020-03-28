module FunctorialLogic where
{--
  really forAll and thereExists should be defined for parts of dom(f)
  as they are rather trivial for points.
  forAll :: (s -> b) -> [s] -> [b]
  thereExists :: (s -> b) -> [s] -> [b]
--}

class Logical s where
  thereExists :: (s -> b) -> s -> b
  forAll :: (s -> b) -> s -> [b]
  thereExists f a = f a
  forAll f a = undefined

  invImg :: Eq b => (s -> b) -> b -> [s]
  domain :: [s]

instance Logical Char where
  invImg f b = [a | a <- ['a'..], f a == b]
  domain = ['a'..]

instance Logical Int where
  invImg f b = [a | a <- domain, f a == b]
  domain = [0..] ++ map negate [0..]

intEx :: [Int]
intEx = invImg (\ z -> z `mod` 10^5) 2

charEx = invImg (\ c -> c:"hars") "ahars"
