-- an attempt to encode the quantifiers

import Control.Monad
import Data.Set hiding (map, foldr, foldl)

data Fiber v = V [v] | Fiber deriving (Show, Eq, Ord)
data UxV u v = P u (Fiber v) deriving (Show)

type Fibers u v = [UxV u v]

example = P 2 Fiber
another = P 1 (V [2,3,4])
them = fibers [7,7,7] [1,2,3]
up_two = lift another [1,1,1]

pr1 (P a b) = a
pr2 (P a b) = b

-- this makes the full product space.
fibers :: [v] -> [u] -> Fibers u v
fibers vs us = map lift us <*> [vs]

lift u vs = P u (V vs)

--perhaps fmap should modify the fiber
-- kind UxV u v is no good for functor.
-- instance Functor ((UxV u) v) where
  -- fmap f g = (\vs -> (g f))