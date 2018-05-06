module RayTracer.TransportSketch where
import RayTracer.RegionDetection
import RayTracer.FileToVector
import RayTracer.RayLength

fractional :: Double -> Double
fractional = snd.properFraction

arraySize = 10.0::Double

-- exit point is arraySize dependent.
exitCond x theta size =
  case (exitRegion x theta size) of
    LeftSide  -> (> 0)
    Top       -> (< size - 1)
    RightSide -> (< size - 1)

-- ranges for crossings.
xsNegSlope x = [0..(fromIntegral.floor) x]
ysNegSlope x th = [0..(fromIntegral.floor)(-x * tan th)]

xsPosSlope th x = [(fromIntegral.ceiling) x..(arraySize/tan th + x)]
ysPosSlope x th = [0..(fromIntegral.floor) $ (arraySize - x) * tan th] -- floor?

-- ys values at integer x.
xcrossings x theta size =
  let ypt k = (k - x) * tan theta in
  let rLen k = sqrt $ (k-x)**2 + (ypt k)**2 in
  let range = if tan theta < 0 then xsNegSlope else xsPosSlope theta in
  [(ypt k, rLen k) | k <- range x]

-- xs values at integer y.
ycrossings x theta size =
  let xpt k = x + k / tan theta in -- this part is right.
  let rLen k = sqrt $ k**2 + (xpt k - x)**2  in -- this part seems off.
  let range = if tan theta < 0 then ysNegSlope else ysPosSlope in
  [ (xpt k, rLen k) | k <- range x theta]


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
