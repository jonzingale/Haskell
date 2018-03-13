module FixAndFriends where

badGuy = undefined

-- enumerate all rational numbers
fix :: (a -> a) -> a
fix f = let {x = f x} in x

rationals = fix ((1%1 :) . (>>= \x -> [x+1, 1/(x+1)]))
-- [1%1,2%1,1%2,3%1,1%3,3%2,2%3,4%1,1%4,4%3,3%4,5%2,2%5,...]

integers = fix ((0 :) . (>>= \x -> [x+1, x])) -- so freaking wrong.
-- https://wiki.haskell.org/Blow_your_mind