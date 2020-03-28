module PosetLogic where
import Prelude hiding (map, filter)
import Data.Set

type PowerSet s = Set (Set s)

class (Eq s, Ord s) => Logical s where
  domain :: PowerSet s

  exists :: Ord b => (Set s -> Set b) -> PowerSet s -> PowerSet b
  exists f = map f

  -- A GUIDE:
  -- forAll ::Eq b => (s -> b) -> [s] -> [b]
  -- forAll f as = [ f x | x <- invImg f (map f as), elem x as ]

  -- forAll ::Eq b => (s -> b) -> Set s -> Set b
  -- forAll f as = [ DA.map f x | x <- invImg f (DA.map f as), elem x as ]

  invImg :: (Eq b, Ord b) => (Set s -> Set b) -> PowerSet b -> PowerSet s
  invImg f setB = filter (\a -> member (f a) setB) $ domain

  incl :: [s] -> PowerSet s
  incl = powerSet.fromList

instance Logical Char where
  domain = incl ['a'..]

-- Examples
invCharEx :: PowerSet Char -- times out hard
invCharEx = invImg (\ c -> union c (fromList "hars")) $ incl "ab"

-- allCharEx = forAll (\ c -> "s") "abcs"
-- existsCharEx = exists (\ c -> c:"hars") "abcs"