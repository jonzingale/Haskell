module DynGenerator where
import Data.Matrix --  identity, permMatrix, fromLists, toLists

{--
NOTES:
Dyn representations as 1,0 valued adjacency matrices whose rows sum to 1.

:t \x y -> (*) <$> x <*> y
(*) <$> [[0,1],[1,0]] <*> [[0,1],[1,0]] => error

cabal install --lib matrix-0.3.6.0

Look at splitting and joining blocks for building tensor?
--}

{--
TODO:
- newtype or data?
- tensors? none in library!
--}

ex0 = identity 2
ex1 = matrix 4 4 $ \(i,j) -> 2*i - j
ex2 = fromLists [[0,1],[1,0]]

tensor :: Num a => [a] -> [a] -> [a]
tensor =  \x y -> (*) <$> x <*> y

val = fromList 4 4 $ tensor (toList ex2) (toList ex0)