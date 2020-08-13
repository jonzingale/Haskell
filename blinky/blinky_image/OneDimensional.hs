module OneDimensional where
import Comonad

{-- 1D Cellular Automata --}
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

run1d :: IO ()
run1d = do
  let states = iterate (blink sierpinski) seed1d
  let limitedRun = take 24 states
  putStr $ unlines.map show $ limitedRun
