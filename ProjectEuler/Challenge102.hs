module Challenge102 where
import Triangles

{--
4 Quadrants

II,  I
III, IV

calculate y and x intercepts for each line.
verify that intercept exists on segment.
each intercept straddles two regions, or all regions.
verify that all four regions are present.
--}

euler102 = length.filter allQuads $ triangles

data Quadrant = I | II | III | IV deriving (Eq, Show)
data Segment = S Point Point deriving (Eq, Show)
type Triangle = [(Double, Double)]
type Point = (Double, Double)

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

edges :: Triangle -> [Segment]
edges [a, b, c] = [S a b, S a c, S b c]

xycept :: [Segment -> Point]
xycept = [xcept, ycept]
  where
    ycept (S (a,b) (c,d)) = (0 ,(a*d - c*b) / (a-c))
    xcept (S (a,b) (c,d)) = ((c*b - a*d) / (b-d), 0)

lineMember :: Segment -> Point -> Bool
lineMember (S (a,b) (c,d)) (p,q) | a < c = f a c p
                                 | otherwise = f c a p
  where
    f x y t = and [x<t, t<y] 

allQuads :: Triangle -> Bool -- in all Quads
allQuads tri = (length.ceptData) tri == 4

ceptData :: Triangle -> [Quadrant]
ceptData tr = cleanConcat $ endToQuad tr : (map quadCepts (edges tr))
  where
    cleanConcat = remdups.concat
    remdups [] = []
    remdups (x:xs) = x : remdups [t | t<-xs, t /= x]

quadCepts :: Segment -> [Quadrant]
quadCepts seg = f $ xycept <*> [seg]
  where
    f = concat.(map ceptToQuad).filter (lineMember seg) 

throughOrigin :: Triangle -> Bool -- False for all!
throughOrigin tri = any (== (0,0)) $ xycept <*> edges tri




