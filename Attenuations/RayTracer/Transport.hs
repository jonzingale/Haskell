module RayTracer.Transport where
import RayTracer.FileToVector

type Coords = (XCoord, YCoord)
type SegmentLength = Double
type XCoord = Double
type YCoord = Double
type Angle  = Double

-- displayTrace 2.3 (pi/5)
displayTrace x t = do
  ary <- fileToAry "./Tests/data49Doubles"
  let (_:ijSeg) = transport x t -- because the head is not necessary.
  let eval = [ seg * (qArray 7 ij ary) | (ij, seg) <- takeWhile stopCond ijSeg]
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
transport :: XCoord -> Angle -> [((Int, Int), SegmentLength)]
transport x theta =
  let xcs = xcrossings x theta in
  let ycs = ycrossings x theta in
  let nudge = if theta > pi/2 then fromIntegral.ceiling else fromIntegral.floor in
  let sign = if theta > pi/2 then -1 else 1 in
  f xcs ycs (x, 0) (nudge x, -1) sign -- floor for negative slope case only
  
  where -- xcs ycs (p,q) (i,j)
    f ((xh,yh): xcs) ((xv,yv): ycs) pt (i, j) sign
      | yh < yv = ((i,j), segment pt (xh,yh)) : f xcs ((xv,yv): ycs) (xh,yh) (i+sign, j) sign
      | otherwise = ((i,j), segment pt (xv,yv)) : f ((xh,yh): xcs) ycs (xv,yv) (i, j+1) sign

-- xcrossings are dependent on either θ < π/2 or θ > π/2.
-- These y values should always go positive. There likely
-- hides a symmetry about pi/2.
xcrossings :: XCoord -> Angle -> [(XCoord, YCoord)]
xcrossings x theta
  | theta > pi/2 = [(ff x - k, -(frac x + k)*tan theta) | k<-[0..]]
  | theta < pi/2 = [(ff x + k + 1, (1 - frac x + k)*tan theta) | k<-[0..]]
  | otherwise = [] -- Is this right?
  where frac = snd.properFraction

-- ycrossings are dependent on either θ < π/2 or θ > π/2.
-- These x values may go negative. All three cases the same!
ycrossings :: XCoord -> Angle -> [(XCoord, YCoord)]
ycrossings x theta = [ (x + k / tan theta, k) | k <- [0..]]

segment :: Coords -> Coords -> SegmentLength
segment (x1, y1) (x2, y2) = sqrt $ (x2-x1)**2 + (y2-y1)**2

cc, ff :: Double -> Double
cc = fromIntegral.ceiling
ff = fromIntegral.floor