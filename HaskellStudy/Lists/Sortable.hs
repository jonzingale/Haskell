module Sortable (sort, shuffle) where
import Control.Monad.Zip
import System.Random
import Listable

randos :: [Integer]
randos = randomRs (0, 10^6) $ mkStdGen 32

class (Ord s, Listable s) => Sortable s where
  sort, shuffle :: s -> s

  sort ns | ns == unit = unit
          | otherwise = branch smaller ns +++ headL ns +++ branch larger ns
    where
      branch f xs = sort.f (headL xs) $ tailL xs
      smaller n ns = filterL (<= n) ns
      larger n ns  = filterL (>  n) ns

  shuffle ns = eval . (map snd) . sort . zipS randos $ ns
    where
      zipS rands s = f rands s
      f [] s = []
      f (x:xs) s | s == unit = []
                 | otherwise = (x, headL s) : f xs (tailL s)
      eval [] = unit
      eval (a:as) = a `cons` eval as

instance Sortable Integer where
instance Ord a => Sortable [a] where


