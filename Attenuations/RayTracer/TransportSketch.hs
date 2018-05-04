module RayTracer.TransportSketch where
import RayTracer.RegionDetection
import RayTracer.FileToVector
import RayTracer.RayLength

fractional :: Double -> Double
fractional = snd.properFraction

arraySize = 10 -- <--- another error prone spot.

-- length diffs
tourists x th = take 4 $ zip (xcrossings x th arraySize)
                             (ycrossings x th arraySize)

-- exit point is arraySize dependent.
exitCond x theta size =
  case (exitRegion x theta size) of
    LeftSide  -> (> 0)
    Top       -> (< size)
    RightSide -> (< size)

-- ys values at integer x.
xcrossings x theta size =
  let ypt k = x + k / tan theta in
  let rLen k = sqrt $ k**2 + (ypt k - x)**2 in
  let takeWhileExit = takeWhile $ (exitCond x theta size) . fst in
  takeWhileExit [(ypt k, rLen k) | k <- [0..]]

-- xs values at integer y.
ycrossings x theta size = -- SOMETHING WEIRD: ycrossings 8.1 (2*pi/5)
  let xpt k = (k-x) * tan theta in
  let rLen k = sqrt $ (k-x)**2 + (xpt k)**2 in
  let takeWhileExit = takeWhile $ (exitCond x theta size) . fst in
  let dropWhileExit = dropWhile $ (< 0) . fst in
  takeWhileExit.dropWhileExit $ [ (xpt k, rLen k) | k <- [1..]]

--- close to being pretty good.
-- totalRayLength x theta =
--   f (xcrossings x theta) (ycrossings x theta) arraySize 0
--   where
--     f ((y,r1):xcs) ((x,r2):ycs) s accum
--       | y > s || x > s || y < 0 || x < 0 = accum
--       | r1 < r2 = f xcs ((x,r2):ycs) s r1
--       | otherwise = f ((y,r1):xcs) ycs s r2

-- main = do
  -- myArray <- anArray
  -- return $ totalRayLength x theta
