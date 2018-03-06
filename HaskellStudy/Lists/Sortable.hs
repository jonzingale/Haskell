module Sortable (sort, shuffle) where
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

    -- map or zippable or something. needed for Listable. zippable INteger?
  -- shuffle ns = (map snd).sort.(zip randos) $ ns

instance Sortable Integer where
instance Ord a => Sortable [a] where
