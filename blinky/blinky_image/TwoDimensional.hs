module TwoDimensional where
import System.Random
import Comonad

type Rule = V Int -> Int

conway :: Rule
conway bb =
  let total = sum.mooreNeigh $ bb in
  case (coreturn bb) of
    1 -> if total == 2 || total == 3 then 1 else 0
    0 -> if total == 3 then 1 else 0

sprinkles :: V Int -> Int
sprinkles bb = coreturn bb + (sum.mooreNeigh $ bb) `mod` 2

board :: V Int
board = V (repeat lefts) seed  (repeat rights)
  where
    lefts  = U (repeat 1) 2 (repeat 3)
    seed   = U (repeat 4) 5 (repeat 6)
    rights = U (repeat 7) 8 (repeat 9)

line :: V Int
line = V (repeat zeros) seed  (repeat zeros)
  where
    seed  = U (1:repeat 0) 1 (1:repeat 0)
    zeros = U (repeat 0) 0 (repeat 0)

-- not a good board to start with
rboard :: V Int
rboard = V (repeat (randos 32)) (randos 30) (repeat (randos 31))
  where
    biased s = [ if r == (0::Int) then 1 else 0 | r <- randomRs (0,5) $ mkStdGen s]
    randos s = U (biased s) 0 (biased (s+1))

data V x = V [U x] (U x) [U x]

instance (Monoid a, Eq a) => Show (V a) where
  show (V as b cs) = unlines.map show $
    (take 10 as) ++ [b] ++ (take 10 cs)

instance Functor V where
  fmap f (V a b c) = V (map (fmap f) a) (fmap f b) (map (fmap f) c)

instance Zipper V where
  right (V a b (c:cs)) = V (b:a) c cs
  left  (V (a:as) b c) = V as a (b:c)

-- types step near each others faces
instance Comonad V where
  cojoin a = V (til.f $ a) (f a) (tir.f $ a)
    where
      til = tail.iterate left
      tir = tail.iterate right
      f a = U (tail.iterate left $ a) a (tail.iterate right $ a)
  coreturn (V _ b _) = coreturn b

mooreNeigh :: V x -> [x]
mooreNeigh (V (a:_) b (c:_)) = foldr (++) [] [f a, f b, f c]
  where
    f (U (x:_) y (z:_)) = [x,y,z]
    g (U (x:_) y (z:_)) = [x,z]

run2d :: Rule -> IO ()
run2d rule = do
  let states = iterate (blink rule) line
  let limitedRun = take 24 states
  -- putStr $ unlines.map show $ limitedRun
  putStr $ foldr clsConcat "" $ map show limitedRun
  where clsConcat = (\ x y -> x ++ "\ESC[2J" ++ y)
