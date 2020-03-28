module FunctorialLogic where
import Prelude hiding (map, filter)
import Data.Set

import Data.Char (ord, chr) -- for examples

type PowerSet s = Set (Set s)
type SetMap s b = Set s -> Set b

class Ord s => Logical s where
  domain :: PowerSet s

  exists :: Ord b => SetMap s b -> PowerSet s -> PowerSet b
  exists f = map f

  forAll :: Ord b => SetMap s b -> PowerSet s -> PowerSet b
  forAll f as = let invF = invImg f (map f as) in
    map f $ filter (\x -> member x as) invF

  invImg :: Ord b => SetMap s b -> PowerSet b -> PowerSet s
  invImg f setB = filter (\a -> member (f a) setB) domain

  incl :: [s] -> PowerSet s
  incl = powerSet.fromList

instance Logical Char where
  domain = incl ['a'..'c'] -- keep this small

instance Logical Int where
  domain = incl [97..100] -- keep this small

-- Examples
invCharEx, existsCharEx, allCharEx :: PowerSet Char
invCharEx = invImg ( \i -> map ord i ) $ incl [97, 99]
existsCharEx = exists ( \c -> map chr c ) $ incl [97, 99, 102] -- acf
allCharEx = forAll ( \c -> map chr c ) $ incl [97, 99, 102] -- acf

