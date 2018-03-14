module InfinitePairs where
import Control.Arrow ((***))
import Text.Printf -- for printf
import Sortable
import Listable

data InfList a = Z {left :: [(a, a)], focus :: [(a, a)], right :: [(a, a)]} deriving (Eq, Ord)

instance Show a => Show (InfList a) where
   show (Z a b c) = printf format (ff reverse a) (show b) (ff id c)
    where
      format = "[..%s %s %s..]\n"
      ff f = unwords.(map show).f.flatten.(take 8)
      flatten [] = []
      flatten ((x,y):xs) = (x,y):(flatten xs)

shiftLeft :: InfList a -> InfList a
shiftLeft (Z (a:as) b cs) = Z as [a] (b++cs)

shiftRight :: InfList a -> InfList a
shiftRight (Z as b (c:cs)) = Z (b++as) [c] cs

fibs, ten, randos, ordered :: InfList Integer
ten = limit 10 fibs
randos = Z [(6,2),(4,1)] [(7,9)] [(8,5),(3,0)]
ordered = Z nrandlike [(0,1)] prandlike
  where
    prandlike = zip [1..] [mod (x^100 + 1) 10 | x <- [1..]]
    nrandlike = mapVmap negate prandlike

fibs = Z (map (vmap negate) ffibs) [(0,0)] ffibs
  where
    ffibs = (0,1) : zipWith binOp ((1,0) : ffibs) ffibs
    binOp = \(x,y) (z,w)-> (x+z, y+w)

limit :: Eq a => Integer -> InfList a -> InfList a
limit n (Z a b c) = Z (takeL n a) b (takeL n c)

vmap :: (a -> b) -> (a,a) -> (b,b)
vmap f = f *** f
mapVmap f = map (vmap f)
emap (f,g) (a,b) = (f a, g b)
mapEmap f = map emap f

instance Functor InfList where
  fmap f (Z a b c ) = Z (mapVmap f a) (mapVmap f b) (mapVmap f c)

instance Applicative InfList where
  pure x = Z (repeat (x,x)) [(x,x)] (repeat (x,x))
  (<*>) (Z fs g hs) (Z as b cs) = Z (mapEmap fs <*> as) (mapEmap g <*> b) (mapEmap hs <*> cs)

instance (Ord a, Eq a) => Listable (InfList a) where
  takeL 0 (Z a b c) = unit
  takeL n (Z a b c) = Z (takeL (n-1) a) b (takeL (n-1) c)
  dropL n (Z [] b []) = unit
  dropL n (Z a b c) = Z (dropL n a) b (dropL n c)
  (+++) zs (Z a b c) = Z ((toList zs) ++ a) b c
  -- (+++) (Z a b c) zs = Z a b (c ++ toList zs)
    where
      toList (Z a b c) = a++b++c

  tailL (Z [] b []) = unit
  tailL (Z [] b (c:cs)) = Z [] [c] cs
  tailL (Z (a:as) b c) = Z as [a] c
  cons (Z _ b _) ws = (Z [] b []) +++ ws
  unit = Z [] [] []

instance (Ord a) => Sortable (InfList a) where -- shuffles are wrong, check cons
  qsort (Z a b c) = let n = lengthL a in
                    let sort = qsort $ a++b++c in
      Z ((reverse.takeL n) sort) (sort!!!n) (dropL (n+1) sort)
