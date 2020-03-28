module PosetLogic where
import Prelude hiding (map, filter)
import Data.Set

type PowerSet s = Set (Set s)
type SetMap s b = Set s -> Set b

class Ord s => LogicalPoset s where
  domain :: PowerSet s

  exists :: Ord b => SetMap s b -> PowerSet s -> PowerSet b
  exists f = map f

  forAll :: Ord b => SetMap s b -> PowerSet s -> PowerSet b
  forAll f as = let invF = invImg f (map f as) in
    map f $ filter (\x -> member x as) as

  invImg :: Ord b => SetMap s b -> PowerSet b -> PowerSet s
  invImg f setB = filter (\a -> member (f a) setB) $ domain

  incl :: [s] -> PowerSet s
  incl = powerSet.fromList

instance LogicalPoset Char where
  domain = incl ['a'..]

-- Examples
invCharEx :: PowerSet Char -- times out hard
invCharEx = invImg (\ c -> union c (fromList "abc")) $ incl "ab"

-- allCharEx = forAll (\ c -> "s") "abcs"
-- existsCharEx = exists (\ c -> c:"hars") "abcs"