module DynGenerator where
import Data.Matrix --  identity, permMatrix, fromLists, toLists, nrows, ncols

{--
NOTES:
Dyn representations as 1,0 valued adjacency matrices whose rows sum to 1.

:t \x y -> (*) <$> x <*> y
(*) <$> [[0,1],[1,0]] <*> [[0,1],[1,0]] => error

cabal install --lib matrix-0.3.6.0
--}

{--
TODO:
- newtype or data?
- tensors? none in library!
--}

ex02 = zero 2 2
exi2 = identity 2
ex12 = fromLists [[0,1],[1,0]]

ex0 = zero 4 4
exi = identity 4
ex1 = fromLists [[0,1,1,0],[0,1,1,1],[1,1,1,0],[0,1,1,0]]

-- sanity check
tensorExample = (ex02 <|> exi2) <-> (exi2 <|> ex02)
test = tensor ex12 exi2 == tensorExample

-- Note: square matrices only
tensor :: Num a => Matrix a -> Matrix a -> Matrix a
tensor m1 m2 =
  let n = nrows m1 in
  let ms = toLists m1 in
  let mparts = map (map (\i -> scaleMatrix i m2)) ms in
  process mparts n
  where
    -- could probably be written better, another foldr on (<->)?
    process (p:[]) i =
      foldr (<|>) (last p) (take (i-1) p)
    process (p:ps) i =
      foldr (<|>) (last p) (take (i-1) p) <-> process ps i
