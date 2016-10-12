-- an attempt to encode the quantifiers

{-- Todo:
  *lift things correctly.
  *understand the dependence on Dom, Cod
  *extend to arbitrary mappings via functor class.

  It would be best to not have to evaluate anything
  until a proper S on P(uxv) is given. This can be
  accomplished for ∀ by noticing that it is all of
  the diagonal elements of P(uxv).

--}
import Control.Monad
import Data.Set hiding (map, foldr, foldl)
{--
singleton :: a -> Set a
fromList :: Ord a => [a] -> Set a

insert :: Ord a => a -> Set a -> Set a
isSubsetOf :: Ord a => Set a -> Set a -> Bool

intersection :: Ord a => Set a -> Set a -> Set a
difference :: Ord a => Set a -> Set a -> Set a
union :: Ord a => Set a -> Set a -> Set a

elems :: Set a -> [a]
empty :: Set a
--}

data V = One | Two | Three | Four deriving (Show, Eq, Ord)
data VxS a = P [a] | Empty deriving (Show)
type Fibers a = [VxS a]

example :: Fibers Integer
example = fibers [1,2,3]

way_high :: Fibers (VxS Integer)
way_high = fibers.fibers $ [1,2,3]

vs = [One, Two]

-- map eval $ fibers [1,2,3]
eval (P us) = [ (x, y) | x <- us, y <- vs]
eval Empty = []

pr1 (P a) = a
pr1 Empty = []

prjs :: (Settable t) => Fibers t -> [t]
prjs xs = foldl (/\) [] $ map pr1 $ xs

fibers :: (Settable a) => [a] -> Fibers a -- Can this be a -> Fibers a ?
fibers xs = map (P . (:[])) xs -- ⊥⊥⊥
-- p_star = fibers

test_id = example == (prjs.fibers) example

glue :: (Settable a) => Fibers a -> VxS a
glue = P . prjs

powerset :: Ord a => VxS a -> Fibers a
powerset xs = map P $ filterM (\x -> [True, False]) (pr1 xs)

-- Build a comonad on fibers.
-- counit wa -> a
counit fibs = foldl (/\) [] $ map pr1 $ fibs

-- cobind (w a) -> (w a -> b) -> w b
-- (==<) :: (Settable b, Settable a) => [VxS a] -> (VxS a x -> b) -> [VxS b]
-- fibs ==< f = fmapF f (fibers fibs)

fmapP :: Ord a => (a1 -> a) -> VxS a1 -> VxS a
fmapP f = ((P).map f) . pr1

fmapF :: Ord a => (a1 -> a) -> Fibers a1-> Fibers a
fmapF f = map (fmapP f)

test_func (P a) = (/\) a [3]

------ Class Definitions for Settable
instance (Ord a, Show a) => Eq (VxS a) where
  (P a) == (P b) = a ~~ b

instance Settable a => Ord (VxS a) where
  (P a) < (P b) = isSubsetOf (fromList a) (fromList b)
  (P a) > (P b) = isSubsetOf (fromList b) (fromList a)
  (<=) = (<)
  (>=) = (>)

instance Settable Integer where
  a ->> b = a < b
  a <<- b = a > b
  a ~~ b = a == b
  a /\ b = lcm a b
  a \/ b = gcd a b

instance (Ord a, Show a, Eq a) => Settable [a] where
  a ->> b = isSubsetOf (fromList a) (fromList b)
  a <<- b = isSubsetOf (fromList b) (fromList a)
  a ~~ b = a ->> b && b ->> a
  a /\ b = toList $ union (fromList a) (fromList b)
  a \/ b = toList $ intersection (fromList a) (fromList b)

instance Settable a => Settable (VxS a) where
  (P a) ->> (P b) = a ->> b
  (P a) <<- (P b) = a <<- b
  (P a) ~~ (P b) = a ->> b && b ->> a
  (P a) /\ (P b) = P $ a /\ b
  (P a) \/ (P b) = P $ a \/ b

class (Eq s, Ord s, Show s) => Settable s where
  (~~) :: s -> s -> Bool
  (->>) :: s -> s -> Bool
  (<<-) :: s -> s -> Bool
  (/\) :: s -> s -> s
  (\/) :: s -> s -> s