module PosetLogic where
import qualified Data.Set as DA
import Data.Set

-- to unwrap powerset of things.
unwrap x = Prelude.map toList $ toList x

class Eq s => Logical s where
  exists :: Ord b => (s -> b) -> Set s -> Set b
  exists f = DA.map f

  -- forAll ::Eq b => (s -> b) -> Set s -> Set b
  -- forAll f as = [ DA.map f x | x <- invImg f (DA.map f as), elem x as ]

  invImg :: (Eq b, Ord b) => (s -> b) -> Set b -> Set s
  invImg f setB = DA.filter (\a -> member (f a) setB) domain

  domain :: Set s

instance Logical Char where
  domain = fromList ['a'..]

instance Logical Int where
  domain = fromList $ [0..] ++ Prelude.map negate [0..]

intEx :: Set Int -- times out!!!
intEx = invImg (\ z -> z `mod` 10) $ singleton 2

invCharEx :: Set Char
invCharEx = invImg (\ c -> c:"hars") $ fromList ["a", "bc"]

-- allCharEx = forAll (\ c -> "s") "abcs"
-- existsCharEx = exists (\ c -> c:"hars") "abcs"