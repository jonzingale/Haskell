module TwoDimensional where
import Comonad

{-- 2D Cellular Automata --}
type Rule = V Int -> Int

line :: V Int
line = V (repeat zeros) seed  (repeat zeros)
  where
    seed  = U (1:repeat 0) 1 (1:repeat 0)
    zeros = U (repeat 0) 0 (repeat 0)

conway :: Rule
conway bb =
  let total = sum.neigh $ bb in
  case (coreturn bb) of
    1 -> if total == 2 || total == 3 then 1 else 0
    0 -> if total == 3 then 1 else 0

sprinkles :: Rule
sprinkles bb = (coreturn bb + (sum.neigh $ bb)) `mod` 2

neigh :: V x -> [x]
neigh (V (a:_) b (c:_)) = foldr (++) [] [f a, g b, f c]
  where
    f (U (x:_) y (z:_)) = [x,y,z]
    g (U (x:_) y (z:_)) = [x,z]

run2d :: Rule -> IO ()
run2d rule = do
  let states = iterate (blink rule) line
  let limitedRun = take 24 states
  -- putStr $ unlines.map show $ limitedRun
  putStr $ foldr clsConcat "" $ map show limitedRun
  where clsConcat = (\x y -> x ++ "\ESC[2J" ++ y)
