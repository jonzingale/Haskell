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

negEx = [-k * tan (4*pi/5)| k<-[0..(fromIntegral.floor) 6]]
-- posEx = [(k-6) * tan (pi/5)| k<-[(fromIntegral.ceiling) 6..(9/tan (pi/5) + 6)]]
-- posEx' = [k * tan (pi/5)| k<-[0..(9/tan (pi/5))]]

-- ranges for crossings. # Warning: ranges need orienting
xsNegSlope x = [0..(fromIntegral.floor) x]
ysNegSlope x th = [0..(fromIntegral.floor)(-x * tan th)]

-- xsPosSlope th s x = [(fromIntegral.ceiling) x..(s/tan th + x)]
xsPosSlope th s x = [0..s / tan th]
ysPosSlope s x th = [0..(fromIntegral.floor) $ (s - x) * tan th] -- floor?

-- ys values at integer x.
xcrossings x theta size =
  -- let ypt k = (k - x) * tan theta in -- 
  let ypt k = k * tan theta in -- 
  let yps k = -k * tan theta in
  let ys = if tan theta < 0 then yps else ypt in
  let rLen k = sqrt $ (k-x)**2 + (ypt k)**2 in
  let range = if tan theta < 0 then xsNegSlope else xsPosSlope theta size in
  [(ys k, rLen k) | k <- range x]

-- xs values at integer y.
ycrossings x theta size =
  let xpt k = x + k / tan theta in -- this part is right.
  let rLen k = sqrt $ k**2 + (xpt k - x)**2  in -- this part seems off.
  let range = if tan theta < 0 then ysNegSlope else ysPosSlope size in
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
