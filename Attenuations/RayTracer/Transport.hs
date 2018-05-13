module RayTracer.AnotherTransportSketch where
-- import RayTracer.TransportHelpers
-- import RayTracer.RegionDetection
import RayTracer.FileToVector
-- import RayTracer.RayLength

import Data.Array
import System.Random

type Coords = (XCoord, YCoord)
type SegmentLength = Double
type Angle = Double
type XCoord = Double
type YCoord = Double
type Direction = String

testArray = listArray (1,49) randos
  where
    randos = take 49 $ randomRs (0,1::Double) $ mkStdGen 42 

toPxl :: (Int, Int) -> Int
toPxl (x,y) = x + y * 7

-- returns the ordered segment lengths, needs array hookups
towardArrays :: XCoord -> Angle -> [((Int, Int),SegmentLength)]
towardArrays x theta =
  let xcs = xcrossings x theta in
  let ycs = ycrossings x theta in
  let nudge = if theta > pi/2 then fromIntegral.ceiling else fromIntegral.floor in

  f xcs ycs (x, 0) (nudge x, -1) -- floor for negative slope case only
  
  where -- xcs ycs (p,q) (i,j)
    f ((xh,yh): xcs) ((xv,yv): ycs) pt (i, j)
      | yh < yv = ((i,j), segment pt (xh,yh)) : f xcs ((xv,yv): ycs) (xh,yh) (i+1, j)
      | otherwise = ((i,j), segment pt (xv,yv)) : f ((xh,yh): xcs) ycs (xv,yv) (i, j+1)

css x t = do
  putStr "xs at y crossings:\n(x val, y)\n"
  putStr.unlines.(take 7).(map show) $ ycrossings x t
  putStr "\nys at x crossings:\n(x, y val, pn+1 - pn)\n"
  putStr.unlines.(take 7).(map show) $ xcrossings x t

anOrdList x t = do
  putStr "(X,Y) EntryDirection SegmentLength\n"
  let (_:ijSeg) = take 6 $ towardArrays x t
  let eval = [ seg * (testArray!(toPxl ij)) | (ij, seg) <- ijSeg]
  putStr.unlines.(map show) $ eval
{--
Given that the slope is positive and both dispensers
(xcrossings, ycrossings) are increasing, each can be
taken in turn.

Following y-values rather than the x-values ought to
simply the counting rules. y-values are always increasing
from 0.
--}

-- take 10 $ anOrderedList 2.3 (pi/5)
anOrderedList :: XCoord -> Angle -> [(XCoord, YCoord, Direction, SegmentLength)]
anOrderedList x theta =
  let xcs = xcrossings x theta in
  let ycs = ycrossings x theta in
  f xcs ycs (x, 0)
  where
    f ((xh,yh): xcs) ((xv,yv): ycs) pt
      | yh < yv = (xh,yh,"Side",segment pt (xh,yh)) : f xcs ((xv,yv): ycs) (xh,yh)
      | otherwise = (xv,yv,"Top",segment pt (xv,yv)) : f ((xh,yh): xcs) ycs (xv,yv)



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
segment :: Coords -> Coords -> Double
segment (x1, y1) (x2, y2) = sqrt $ (x2-x1)**2 + (y2-y1)**2

cc, ff :: Double -> Double
cc = fromIntegral.ceiling
ff = fromIntegral.floor