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
-- rewrite this for Integers, ACCURACY!
xs (n,d) = zip [ norm.xcept n k $ d | k <- [1..40]] $ repeat "x"
  where
    fi = fromInteger
    xcept n k d = (fi k, (n*(fi k))/ d)

ys (n,d) = zip [ norm.ycept n k $ d| k <- [1..40]] $ repeat "y"
  where
    fi = fromInteger
    ycept n k d = ((d*(fi k))/ n, fi k)

themGuys (n,d) = foldr (++) "" $ map snd $ sort (xs (n,d) ++ys (n,d))

norm :: (Double, Double) -> Double
norm (x,y) = sqrt (x^2 + y^2)

main = do
  let pMap f pairs = [(p, f p) | p<-pairs]
  let them = pMap themGuys randPairs
  (putStr.unlines.map show) them 

-----------

ex1419 = "xYxYxxYxYxYxxYxYxYxxYxYxYxxY"
ex1419' = [((0,14),(1,5)),((1,9),(2,10)),
           ((2,4),(4,1)),((2,18),(5,6))]

ff (n,d) = f (n,d) (n*d) 2 2
  where
    f (x,y) k i j | mod x k < mod y k = 'x' : f (mod (i*x) k, y) k (i+1) j
                  | otherwise = 'Y' : f (x, mod (j*y) k) k i (j+1)

maxMin (n,d) =
  let xs = [mod (14*k) (19*14) | k<-[1..10]] in
  let ys = [mod (19*k) (14*19) | k<-[1..10]] in
  let zs = zip xs ys in
  zs

-- 3s and 5s
rabbits (n,d) = '.' : f n d 1 1
  where
    f n d i j | n*i < d*j = 'x' : f n d (i+1) j
              | n*i > d*j = 'Y' : f n d i (j+1)
              | otherwise = '.' : f n d (i+1) (j+1)








