
### Project Euler Problem: 102

Three distinct points are plotted at random on a Cartesian plane,<br>for which -1000 ≤ x, y ≤ 1000, such that a triangle is formed.

Consider the following two triangles:

`A(-340,495), B(-153,-910), C(835,-947)`

`X(-175,41), Y(-421,-714), Z(574,-645)`

It can be verified that triangle `ABC` contains the origin, whereas triangle `XYZ` does not.

Using triangles.txt, a text file containing the co-ordinates of one thousand "random" triangles,<br>find the number of triangles for which the interior contains the origin.

### Solution: ###

The goal is to build a set $\Delta = \{t \mid t \in triangles \land \mathbb{0} \in t \}$ of those triangles which include the origin.<br>
Here is an outline of the basic strategy:<br>
- Calculate y and x intercepts for each line.
- Verify that intercept exists on the segment.
- Denote each region that an intercept straddles.
- Verify that all four regions are present, for a given triangle.




First, the data needs to be imported and stored as a usable data structure.<br>
I begin by importing `triangles.txt` as a csv and representing each triangle as a list of pairs.


```haskell
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import Data.List
import Data.Csv

csv <- BL.readFile "triangles.txt"
parsedCsv = decode NoHeader csv :: Either String (Vector [Double])

triangles = map f $ toList.fromRight empty $ parsedCsv
    where
        f [a,b,c,d,e,f] = [(a,b),(c,d),(e,f)]
```

Next, I assign `[(Double, Double)]` a type synonym `Triangle`,<br>
assign `(Double, Double)` a type synonym `Point`, and create datatypes<br>
`Quadrant` and `Segment`.


```haskell
data Quadrant = I | II | III | IV deriving (Eq, Show)
data Segment = S Point Point deriving (Eq, Show)
type Triangle = [(Double, Double)]
type Point = (Double, Double)
```

Next come a pair of functions. `endToQuad` provides a method for determining<br>
which quadrant an endpoint belongs. `ceptToQuad` provides a method for determining<br>
which quadrants an x- or y-intercept straddles.


```haskell
endToQuad :: Triangle -> [Quadrant]
endToQuad [] = []
endToQuad (pt:pts) = f pt : endToQuad pts
  where
    f (x,y) | and [x >= 0, y >= 0] = I
    f (x,y) | and [x <= 0, y >= 0] = II
    f (x,y) | and [x <= 0, y <= 0] = III
    f (x,y) | otherwise = IV

ceptToQuad :: Point -> [Quadrant]
ceptToQuad (0,0) = [I, II, III, IV]
ceptToQuad (0,y) | y > 0 = [I, II]
                 | otherwise = [III, IV]
ceptToQuad (x,0) | x > 0 = [I, IV]
                 | otherwise = [II, III]
```

Next, I define functions `intercepts` and `lineMember`. The first to determine<br>
the x- and y-intercepts of a line extending a given line segment. The second<br>
to determine when the intercept is a member of a given line segment. The helper<br>
`edges` is a convenience method for translating a given `Triangle` to an array<br>
of line `Segment`s.


```haskell
intercepts :: [Segment -> Point]
intercepts = [xcept, ycept]
  where
    ycept (S (a,b) (c,d)) = (0 ,(a*d - c*b) / (a-c))
    xcept (S (a,b) (c,d)) = ((c*b - a*d) / (b-d), 0)

lineMember :: Segment -> Point -> Bool
lineMember (S (a,b) (c,d)) (p,q) | a < c = f a c p
                                 | otherwise = f c a p
  where
    f x y t = and [x<t, t<y] 

edges :: Triangle -> [Segment]
edges [a, b, c] = [S a b, S a c, S b c]
```

Now for the heavy lifting. `quadCepts` takes a line segment and calculates which<br>
intercepts (if any) belong to the segment. If an intercept does belong, the associated<br>
quadrants are accumulated and returned. `allQuadrants` then concatenates to this list of<br>
quadrants those quadrants belonging to the segments’ endpoints. This list is then<br>
reduced by removing any duplicates. Lastly, with this information in hand, `hasOrigin`<br>
verifies whether a given triangle meets the criteria of existing in all four quadrants.


```haskell
hasOrigin :: Triangle -> Bool -- in all Quads
hasOrigin tri = (length.allQuadrants) tri == 4

allQuadrants :: Triangle -> [Quadrant]
allQuadrants tr = remdups.concat $ fullList tr
  where
    remdups [] = []
    remdups (x:xs) = x : remdups [t | t<-xs, t /= x]
    fullList t = endToQuad t : (map quadCepts (edges t)) 

quadCepts :: Segment -> [Quadrant]
quadCepts seg = f $ intercepts <*> [seg]
  where
    f = concat.(map ceptToQuad).filter (lineMember seg)
```

Putting this altogether, `euler102` returns the length of a list of all triangles filtered by `hasOrigin`.



```haskell
euler102 = length.filter hasOrigin $ triangles

```


```haskell
euler102
```


    228

