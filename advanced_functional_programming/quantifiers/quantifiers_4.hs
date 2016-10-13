-- an attempt to encode the quantifiers
import Control.Monad
import qualified Data.Set as S

data UxV u = P u [u] deriving (Show)
type Fibers u = [UxV u]
type Z = Integer

them = p_star [7,7,7] [1,2,3]
up_two = lift another them
another = P 1 [2,3,4]

numbers :: Fibers Z
numbers = p_star [2..10] [2..20]

eval (P u v) = [ (u, y) | y <- v]
eval_fibers = concat . map eval

pr1 (P a b) = a
pr2 (P a b) = b

p_star :: [u] -> [u] -> Fibers u
p_star vs us = map lift us <*> [vs]

lift u vs = P u vs

fermat :: Integral n => (n, n) -> Bool
fermat (p, a) | mod a p == mod (a^p) p = True
              | otherwise = False

_S :: ((t, t) -> Bool) -> [UxV t] -> [(t, t)]
_S f fibered = filter f $ eval_fibers fibered

fermat_S = _S fermat numbers

-- direct_image
exists :: Ord a => [(a, b)] -> [a]
exists someS = S.toList $ S.fromList $ map fst someS

-- glb cylinder in S
forall :: (Eq b, Eq t) => [(b, t)] -> Fibers t -> [b]
forall someS (fibs:xs) = hh someS
  where
    roundup [] = []
    roundup ss = ff ss : roundup (gg ss)
    ff ((x,y):tt) = (x, y : ((snd.unzip.takeWhile ((== x).fst)) tt))
    gg ((x,y):tt) = dropWhile ((== x).fst) tt
    hh = (map fst . filter (\(x,y)->y == pr2 fibs) . roundup) 