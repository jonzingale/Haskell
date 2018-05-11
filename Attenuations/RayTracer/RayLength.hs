module RayTracer.RayLength where

{--
Finds the ray length through a cell.
--}

type RayLength = Point -> Angle -> Double
type Point = (Double, Double) -- valid between 0 and 1
type XPoint = Double -- valid between 0 and 1
type Angle = Double -- valid between 0 and pi

rayLength :: RayLength
rayLength (x,0) = xregion (x,0)
rayLength (0,y) = pkCondition (0,y)
rayLength (1,y) = kpCondition (1,y)

{--
X Cases:

 εδ γ βα
η_\\|//_η
--}

delta (x,0) theta = 1 / sin theta
beta  (x,0) theta = 1 / sin theta

epsilon (x,0) theta | theta == 0 || theta == pi = 1
                    | otherwise = negate x / cos theta

alpha (x,0) theta | theta == 0 || theta == pi = 1
                  | otherwise = (1-x) / cos theta

-- neither of these are necessary.
eta (x,0) theta | theta == pi || theta == 0 = 1
                | otherwise = 0

gamma (x,0) theta | theta == pi/2 = 1
                  | otherwise = 0

{--
Conditions:
--}

xregion :: RayLength
xregion cs theta | theta <= pi / 2 = abCondition cs theta
                 | theta <= pi = edCondition cs theta
                 | otherwise = 999 -- a bad value

-- ε-δ transition
edCondition :: RayLength
edCondition (x, 0) theta | cond x theta = delta (x, 0) theta
                         | otherwise = epsilon (x, 0) theta
  where
    cond x t = (pi/2 + x*pi/4) > t

-- α-β transition
abCondition :: RayLength
abCondition (x, 0) theta | cond x theta = alpha (x, 0) theta
                         | otherwise = beta (x, 0) theta
  where
    cond x t = (pi/4 + x*pi/4) > t


{--
Y Cases:
A good first approximation can be made by determining
which side the ray comes in from and then rotating to
its corresponding x-orientation. x=0 -> 90, x=1 -> 270.

 ρ
|/_κ
--}
rho (0,y) theta | theta == pi/2 || theta == 0 = 1 -- the mu/iota case
                | otherwise = (1-y) / sin theta

kappa (0,y) theta = 1 / cos theta

{--
  ρ'
κ'_\|
--}
rho' (1,y) theta | theta == pi/2 || theta == pi = 1 -- the mu/iota case
                 | otherwise = (1-y) / sin theta

kappa' (1,y) theta = -1 / cos theta

{--
Conditions:
--}
yregion :: RayLength
yregion (x,y) theta = case x of
  0 -> pkCondition (x,y) theta
  1 -> kpCondition (x,y) theta

-- ρ-κ transition
pkCondition :: RayLength
pkCondition (0, y) theta | cond y theta = kappa (0, y) theta
                         | otherwise = rho (0, y) theta
  where
    cond y t = ((1-y)*pi/4) > t -- valid between 0 and pi/2

-- κ'-ρ' transition
kpCondition :: RayLength
kpCondition (1, y) theta | cond y theta = rho' (1, y) theta
                         | otherwise = kappa' (1, y) theta
  where
    cond y t = ((3+y)*pi/4) > t -- valid between pi/2 and pi
