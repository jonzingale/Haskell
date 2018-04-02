module RayTracer where
import Data.List

(n, d) = (4.0, 3.0)

xs = zip [ (n*(fromInteger k))/ d | k<-[1..4]] $ repeat "x"
ys = zip [ (d*(fromInteger k))/ n | k<-[1..4]] $ repeat "y"

thems = sort (xs++ys)
themGuys = foldr (++) "" $ map snd $ sort (xs++ys)


--actually, order by length is the way to go.
