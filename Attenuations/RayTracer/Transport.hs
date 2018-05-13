module RayTracer.AnotherTransportSketch where
import RayTracer.FileToVector

type Coords = (XCoord, YCoord)
type SegmentLength = Double
type XCoord = Double
type YCoord = Double
type Angle  = Double

-- testTrace 2.3 (pi/5)
testTrace x t = do
  ary <- fortyNineDoubles
  let (_:ijSeg) = towardArrays x t
  let eval = [ seg * (qAry49 ij ary) | (ij, seg) <- takeWhile stopCond ijSeg]
  putStr "evaluated total:\n\n"
  putStr.unlines.(map show) $ eval
  where
    stopCond ((x,y), s) = x<=7 && y<=7

{--
Given that the slope is positive and both dispensers
(xcrossings, ycrossings) are increasing, each can be
taken in turn.

Following y-values rather than the x-values ought to
simply the counting rules. y-values are always increasing
from 0.
--}

-- xcrossings are dependent on either θ < π/2 or θ > π/2.
-- These y values should always go positive. There likely
-- hides a symmetry about pi/2.
xcrossings :: XCoord -> Angle -> [(XCoord, YCoord)]
xcrossings x theta
  | theta > pi/2 = [(cc x - k - 1, -(k + frac x)*tan theta) | k<-[0..]] -- verify
  | theta < pi/2 = [(ff x + k + 1, (1 - frac x + k)*tan theta) | k<-[0..]]
  | otherwise = []
  where frac = snd.properFraction

-- ycrossings are dependent on either θ < π/2 or θ > π/2.
-- These x values may go negative. All three cases the same!
ycrossings :: XCoord -> Angle -> [(XCoord, YCoord)]
ycrossings x theta = [ (x + k / tan theta, k) | k <- [0..]]

-- segment (2.3, 0) (3, 0.508)
segment :: Coords -> Coords -> SegmentLength
segment (x1, y1) (x2, y2) = sqrt $ (x2-x1)**2 + (y2-y1)**2

cc, ff :: Double -> Double
cc = fromIntegral.ceiling
ff = fromIntegral.floor