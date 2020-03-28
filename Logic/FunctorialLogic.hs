module FunctorialLogic where
{--
  Really forAll and exists should be defined for parts of dom(f)
  ie. P(S) in P(X). Perhaps begin by specifying Poset with connectives
  or at least a powerset method.
--}

class Eq s => Logical s where
  exists :: (s -> b) -> [s] -> [b]
  exists f as = map f as

  forAll ::Eq b => (s -> b) -> [s] -> [b]
  forAll f as = [ f x | x <- invImg f (map f as), elem x as ]

  invImg :: Eq b => (s -> b) -> [b] -> [s]
  invImg f bs = [ a | a <- domain,  b <- bs, f a == b ]

  domain :: [s]

instance Logical Char where
  domain = ['a'..]

instance Logical Int where
  domain = [0..] ++ map negate [0..]

intEx :: [Int]
intEx = invImg (\ z -> z `mod` 10^5) [2]

invCharEx :: [Char]
invCharEx = invImg (\ c -> c:"hars") ["ahars"]

allCharEx = forAll (\ c -> "s") "abcs"
existsCharEx = exists (\ c -> c:"hars") "abcs"