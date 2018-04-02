module InterceptMethod where

{-- 
Here I calculate the hard way, via the intercepts,
the x and y drifts for many lines. The idea is to
find further evidence for the `up n and then over 1`
algorithm.
--}

data Segment = S Point Point deriving (Eq, Show)
type Point = (Double, Double)

quadCepts :: Segment -> [Point]
quadCepts seg = xycept <*> [seg]

xycept :: [Segment -> Point]
xycept = [xcept, ycept]
  where
    ycept (S (a,b) (c,d)) = (0, (a*d - c*b) / (a-c))
    xcept (S (a,b) (c,d)) = ((c*b - a*d) / (b-d), 0)


seg :: Segment
seg = S (2,0) (15, 7)