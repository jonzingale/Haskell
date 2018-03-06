module Sortable (sort, shuffle) where
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
  sort, shuffle :: s -> s

  sort ns | ns == unit = unit
          | otherwise = branch smaller ns +++ headL ns +++ branch larger ns
    where
      branch f xs = sort.f (headL xs) . tailL $ xs
      smaller n = filterL (<= n)
      larger  n = filterL (>  n)

  shuffle = eval . map snd . sort . zipS randos
    where
      eval = foldr cons unit
      zipS [] s = []
      zipS (x:xs) s | s == unit = []
                    | otherwise = (x, headL s) : zipS xs (tailL s)

instance Sortable Integer where
instance Ord a => Sortable [a] where

