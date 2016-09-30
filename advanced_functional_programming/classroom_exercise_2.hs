import Prelude hiding (zip, zipWith)

data RoseTree a = Node a [RoseTree a] deriving (Show, Eq)
newtype ZipRoseTree a = ZipRoseTree { getZipRoseTree :: RoseTree a }
                                      deriving (Show, Eq)

f_tree :: (a -> b) -> RoseTree (a -> b)
f_tree f = Node f [Node f [], Node f [Node f [], Node f [Node f []]]]

baz = Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 [Node 6 [], Node 7 []] ]
foo = Node 5 [Node 1 [], Node 2 [Node 3 [], Node 4 [Node 5 []]]]
bar = Node 1 [Node 2 [], Node 3 [], Node 4 [], Node 5 []]

instance Functor ZipRoseTree where
  fmap f zRose = ZipRoseTree (fmap f (getZipRoseTree zRose))

instance Applicative ZipRoseTree where
  pure x = ZipRoseTree (pure x)
  f_tree <*> r_tree = let fs = getZipRoseTree f_tree in
                      let rs = getZipRoseTree r_tree in
                      ZipRoseTree $ zipWith eval fs rs
                      where eval f x = f x

instance Functor RoseTree where
  fmap f (Node a []) = Node (f a) []
  fmap f (Node a xs) = Node (f a) $ map (fmap f) xs

instance Applicative RoseTree where
  pure = \x -> Node x []
  (Node f _) <*> xs = fmap f xs

class Zippable z where
  zipWith :: (a -> b -> c) -> z a -> z b -> z c
  (<+>) :: z (b -> c) -> z b -> z c
  (<+>) = zipWith ($)
  zip :: z a -> z b -> z (a,b)
  zip = zipWith (,)

instance Zippable RoseTree where
  zipWith g (Node x rs) (Node y ts) = Node (g x y) $ ziq g rs ts
    where
      ziq f _ [] = []
      ziq f [] _ = []
      ziq f ((Node a as):xs) ((Node b bs):ys) = 
        (Node (f a b) (ziq f as bs)) : ziq f xs ys

class Comonad w where
  counit :: w a -> a
  comult :: w a -> w (w a)
  (<<=) :: w a -> (w a -> b) -> w b

instance Comonad RoseTree where
  counit (Node a xs) = a
  comult (Node a xs) = Node ((Node a xs)) $ map comult xs
  rose_tree <<= f = fmap f $ comult rose_tree

-- Some Tests
fun_up = pure (+1)
fun_dwn = pure (+ (-1))
test = (==) bar $ fun_dwn <*> (fun_up <*> bar)

test_id = foo == foo <<= counit

zip_test = f_tree (+2) <*> foo
zip_id t = (\x -> ((pure id) <*> (ZipRoseTree (pure x))) == (pure x)) t

-- An Idea
thing = fmap (\x -> baz) foo
