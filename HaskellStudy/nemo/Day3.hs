module Day3 where

-- datatype => dataclasses
data Day = Monday | Tuesday | Wednesday | Thursday
  deriving (Show, Eq)

instance Ord Day where
  Monday < Tuesday = True
  Tuesday < Wednesday = True
  Wednesday < Thursday = True
  thursday < Thursday = True

days = [Monday, Tuesday, Wednesday, thursday, Thursday]
abcs = ["a", "b", "c"]
nums = ["1", "2", "3"]

yes :: Bool -> String
yes b
  | b == True = "Yes"
  | otherwise = "Hell No"

bullshit :: Day -> String
bullshit Monday = "Sux"
bullshit _ = "Better"

thursday :: Day
thursday = Monday

example1 :: Day
example1 = [Monday, thursday, Monday] !! 1

numToDay :: Int -> Day
numToDay n
  | n == 0 = Monday
  | n == 1 = Tuesday
  | n == 3 = Wednesday
  | n == 4 = thursday
  | otherwise = Thursday

numToDay' :: Int -> Day
numToDay' 0 = Monday
numToDay' 1 = Tuesday
numToDay' 2 = Wednesday
numToDay' 3 = thursday
numToDay' _ = Thursday

numToDay'' :: Int -> Day
numToDay'' n
  | n < 5 && n >= 0  = days !! n
  | otherwise = Thursday
  | n < 0 = Monday

