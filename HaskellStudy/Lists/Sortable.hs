module Sortable (qsort, keyShuffle, fyShuffle, knuffle) where
import Control.Monad.Zip
import System.Random
import Listable

{--
Any instance of a Listable is immediately extended to be
an instance of Sortable, so long as that Listable is Orderable.
--}

randos :: [Integer]
randos = randomRs (0, 10^6) $ mkStdGen 32

class (Ord s, Listable s) => Sortable s where
  qsort, keyShuffle, fyShuffle, knuffle :: s -> s

  qsort ns | ns == unit = unit
           | otherwise = branch smaller ns +++ headL ns +++ branch larger ns
    where
      branch f xs = qsort.f (headL xs) . tailL $ xs
      smaller n = filterL (<= n)
      larger  n = filterL (>  n)

  fyShuffle xs = f xs randos (lengthL xs) -- OG Fisher-Yates no swap.
    where
      f x _ 0 = unit
      f xs (r:rs) l = let i = mod r l in
        xs!!!i `cons` f (g xs i) rs (l-1)
      g xs i = dropL (i+1) xs +++ takeL i xs

  knuffle xs = f xs randos (lengthL xs) -- Fisher-Yates with swap, Durstenfeld
    where
      f xs _ 0 = xs
      f xs (r:rs) l = f (swap l (mod r l) xs) rs (l-1)
      replace t j xs = dropL (j+1) xs +++ t +++ takeL j xs
      swap i j xs = replace (xs!!!j) i $ replace (xs!!!i) j xs
      -- replace and swap behave differently for lists than for Integer.
      -- the source the problem is that Listable Integers 'work' backwards.

  keyShuffle = eval . map snd . qsort . zipS randos
    where
      eval = foldr cons unit
      zipS [] s = []
      zipS (x:xs) s | s == unit = []
                    | otherwise = (x, headL s) : zipS xs (tailL s)

instance Sortable Integer where
instance Ord a => Sortable [a] where
