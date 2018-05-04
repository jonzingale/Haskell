module RayTracer.RegionDetection where

{--
Finds the exit region of a given ray.
* rescale by arraySize.
* decide exit region.
--}

data Region = LeftSide | Top | RightSide | Bad deriving Show

type RayLength = XPoint -> Angle -> Region
type XPoint = Double -- valid between 0 and 1
type Angle = Double -- valide between 0 and pi

{--
X Cases:

 εδ γ βα
η_\\|//_η
--}

xregion :: RayLength
xregion x theta | theta <= pi / 2 = abCondition x theta
                 | theta <= pi = edCondition x theta
                 | otherwise = Bad

-- ε-δ transition
edCondition :: RayLength
edCondition x theta | cond x theta = Top
                    | otherwise = LeftSide
  where
    cond x t = (pi/2 + x*pi/4) > t

-- α-β transition
abCondition :: RayLength
abCondition x theta | cond x theta = RightSide
                    | otherwise = Top
  where
    cond x t = (pi/4 + x*pi/4) > t

