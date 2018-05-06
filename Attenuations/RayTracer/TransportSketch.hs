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
    Top       -> (< size - 1)
    RightSide -> (< size - 1)

-- ys values at integer x.
xcrossings x theta size =
  let ypt k = (k - x) * tan theta in -- Not sure about this
  let rLen k = sqrt $ (k-x)**2 + (ypt k)**2 in -- not sure about this
  let takeWhileExit = takeWhile $ (exitCond x theta size) . fst in
  takeWhileExit [(ypt k, rLen k) | k <- [initK theta x..fineK x theta size]]
  -- takeWhileExit [(ypt k, rLen k) | k <- [initK theta x..]]

  where
    fineK x theta s = fromIntegral.ceiling $ x + s / (tan theta) -- other case too?
    initK theta | tan theta < 0 = fromIntegral.floor
                | otherwise = fromIntegral.ceiling

-- xs values at integer y.
ycrossings x theta size =
  let xpt k = x + k / tan theta in -- this part is right.
  let rLen k = sqrt $ k**2 + (xpt k - x)**2  in -- this part seems off.
  let takeWhileExit = takeWhile $ (exitCond x theta size) . fst in
  takeWhileExit $ [ (xpt k, rLen k) | k <- [0..]]


-- HELPERS
css x t s = do -- 9.1 (2*pi/5) 10
  putStr "ys at x crossings:\n(y val, ray length)\n"
  putStr.unlines.(map show) $ xcrossings x t s
  putStr "\nxs at y crossings:\n(x val, ray length)\n" 
  putStr.unlines.(map show) $ ycrossings x t s

paramLine :: XPoint -> Angle -> Double -> Double
paramLine x θ = \t -> (t - x) * tan θ

--- close to being pretty good.
totalRayLength x theta size =
  let xcs = xcrossings x theta size in
  let ycs = ycrossings x theta size in
  -- f y@X x@Y size rayTot accum 
  f ((snd.unzip) xcs) ((snd.unzip) ycs) size 0 0-- this could be cleaner.
  where
    aryVal = 1
    f _ [] _ rayTot accum = accum
    f [] _ _ rayTot accum = accum
    f (r1:xcs) (r2:ycs) s rayTot accum
      | r1 < r2 = f xcs (r2:ycs) s r1 ((r1-accum)*aryVal + accum)
      | otherwise = f (r1:xcs) ycs s r2 ((r2-accum)*aryVal + accum)

-- main = do
  -- myArray <- anArray
  -- return $ totalRayLength x theta arraySize
