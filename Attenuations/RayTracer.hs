module RayTracer where
import System.Random
import Data.List

randPairs :: [(Double, Double)]
randPairs = zip (map fromInteger (randos 42)) (map fromInteger (randos 41))
randos seed = randomRs (1, 20) $ mkStdGen seed

{--
Actually, order by length is the way to go.
* calculate intercepts
* calcuate lengths
* order by lengths
--}
-- rewrite as Integers, ACCURACY!
xs (n,d) = zip [ norm.xcept n k $ d | k <- [1..40]] $ repeat "x"
  where
    fi = fromInteger
    xcept n k d = (fi k, (n*(fi k))/ d)

ys (n,d) = zip [ norm.ycept n k $ d | k <- [1..40]] $ repeat "y"
  where
    fi = fromInteger
    ycept n k d = ((d*(fi k))/ n, fi k)

themGuys (n,d) = foldr (++) "" $ map snd $ sort (xs (n,d) ++ ys (n,d))

norm :: (Double, Double) -> Double
norm (x,y) = sqrt (x^2 + y^2)

main = do
  let pMap f pairs = [(p, f p) | p<-pairs]
  let them = pMap themGuys randPairs
  (putStr.unlines.map show) them 

{--
Rythmic technique.
Sarah points out that one only needs compute to
the half-way point and then mirror.
--}

-- length $ take (10^6) $ rabbits (123,101)
-- (1.14 secs, 724,168,432 bytes)
rabbits (n,d) = f n d 0 0
  where
    f n d i j | n*i < d*j = 'L' : f n d (i+1) j
              | n*i > d*j = 'r' : f n d i (j+1)
              | otherwise = '.' : f n d 1 1




