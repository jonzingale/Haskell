module RayTracer.TransportSketch where
import RayTracer.RegionDetection
import RayTracer.FileToVector
import RayTracer.RayLength

fractional :: Double -> Double
fractional = snd.properFraction

arraySize = 10

-- exit point is arraySize dependent.
exitCond x theta size =
  case (exitRegion x theta size) of
    LeftSide  -> (> 0)
    Top       -> (< size)
    RightSide -> (< size)

css x t s = do -- 9.1 (2*pi/5) 10
  putStr "y at x crossing:\n"
  putStr.unlines.(map show) $ xcrossings x t s
  putStr "\nx at y crossing:\n" 
  putStr.unlines.(map show) $ ycrossings x t s

-- ys values at integer x.
xcrossings x theta size =
  let lowerK = fromIntegral.ceiling $ x in
  let ypt k = (k - x) * tan theta in -- Not sure about this
  let rLen k = sqrt $ (k-x)**2 + (ypt k)**2 in -- not sure about this
  let takeWhileExit = takeWhile $ (exitCond x theta size) . fst in
  takeWhileExit [(ypt k, rLen k) | k <- [lowerK..]]

-- xs values at integer y.
ycrossings x theta size =
  let xpt k = x + k / tan theta in -- this part is right.
  let rLen k = sqrt $ k**2 + (xpt k - x)**2  in -- this part seems off.
  let takeWhileExit = takeWhile $ (exitCond x theta size) . fst in
  takeWhileExit $ [ (xpt k, rLen k) | k <- [0..]]


-- HELPERS
tourists x th = take 4 $ zip (xcrossings x th arraySize)
                             (ycrossings x th arraySize)

paramLine :: XPoint -> Angle -> Double -> Double
paramLine x θ = \t -> (t - x) * tan θ

--- close to being pretty good.
totalRayLength x theta =
  let xcs = xcrossings x theta arraySize in
  let ycs = ycrossings x theta arraySize in
  f ((snd.unzip) xcs) ((snd.unzip) ycs) arraySize 0 -- this could be cleaner.
  where
    f _ [] _ accum = accum
    f [] _ _ accum = accum
    f (r1:xcs) (r2:ycs) s _
      | r1 < r2 = f xcs (r2:ycs) s r1
      | otherwise = f (r1:xcs) ycs s r2

-- main = do
  -- myArray <- anArray
  -- return $ totalRayLength x theta
