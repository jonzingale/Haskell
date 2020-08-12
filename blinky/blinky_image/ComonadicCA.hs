module ComonadicCA where
import Comonad

{--
Todo:
1) generalize to the 2D case
2) JuicyPixel output for 1D case
--}

{-- 1D CA --}
seed1d :: U Int
seed1d = U (repeat 0)  1  (repeat 0)

sierpinski :: U Int -> Int
sierpinski (U (a:as) b (c:cs)) =
  case [a, b, c] of
    [0,0,0] -> 0
    [0,0,1] -> 1
    [0,1,0] -> 1
    [0,1,1] -> 1
    [1,0,0] -> 1
    [1,0,1] -> 1
    [1,1,0] -> 1
    [1,1,1] -> 0

ruleToNumber :: [Int] -> Int
ruleToNumber as = foldr (\x y -> 2^x * y) 1 as - 1

numberToRule :: Int -> [Int]
numberToRule 0 = []
numberToRule n = numberToRule (div n 2) ++ [mod n 2]

run1d :: IO ()
run1d = do
  let states = iterate (blink sierpinski) seed1d
  let limitedRun = take 24 states
  putStr $ unlines.map show $ limitedRun
