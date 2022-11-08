module Day5 where

type Name = String

nemo :: Name
nemo = "Nemo"

friendsWithNemo :: Name -> Bool
friendsWithNemo name
  | name == "jon" = True
  | name == nemo = True
  | name == "david" = True
  | otherwise = False

friends :: Name -> Name -> Bool
friends name1 name2
  | name1 == name2 = True
  | (name1 == nemo) || (name2 == nemo) = True
  | name2 == "david" = True
  | otherwise = False

doubleMe :: Int -> Int 
doubleMe x = x + x

maxx :: Ord a => a -> a -> a -> a
maxx a b c = max a (max b c)

maxi :: Ord a => [a] -> a
maxi xs = maximum xs

-- cause its funny
funny :: Int
funny = maxi [8989876, 7646534, 765, 45213556, 896753211236,doubleMe 54332, 768454356] 