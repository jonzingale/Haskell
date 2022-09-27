module PrintHelper where

{--
1. round(2)
2. sort_by_rank

--}

sortRank :: Ord a => [(b, a)] -> [(b, a)]
sortRank [] = []
sortRank ((b,a):bas) =
  sortRank(gte (b,a) bas) ++ [(b,a)] ++ sortRank(lt (b,a) bas)
  where
   lt (x,y) zs = filter (\(q,r) -> r < y) zs
   gte (x,y) zs = filter (\(q,r) -> r >= y) zs