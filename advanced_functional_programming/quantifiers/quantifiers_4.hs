-- an attempt to encode the quantifiers
import Control.Monad

data UxV u = P u [u] deriving (Show)
type Fibers u = [UxV u]
type Z = Integer

another = P 1 [2,3,4]
them = p_star [7,7,7] [1,2,3]
up_two = lift another them

numbers = p_star [2..10] [2..20]

eval (P u v) = [ (u, y) | y <- v]
eval_fibers = concat . map eval

pr1 (P a b) = a
pr2 (P a b) = b

p_star :: [u] -> [u] -> Fibers u
p_star vs us = map lift us <*> [vs]

lift u vs = P u vs

fermat :: Integral n => (n, n) -> Bool
fermat (p, a) | a == mod (a^p) p = True
              | otherwise = False

_S :: ((Z, Z) -> Bool) -> [(Z, Z)]
_S f = filter f $ eval_fibers numbers

fermat_S = _S fermat
