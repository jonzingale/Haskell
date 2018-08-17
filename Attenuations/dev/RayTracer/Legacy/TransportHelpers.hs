module RayTracer.TransportHelpers where
import RayTracer.RayLength

negEx = [-k * tan (4*pi/5)| k<-[0..(fromIntegral.floor) 6]]
posEx = [(k-6) * tan (pi/5)| k<-[(fromIntegral.ceiling) 6..(9/tan (pi/5) + 6)]]
posEx' = [k * tan (pi/5)| k<-[0..(9/tan (pi/5))]]

paramLine :: XPoint -> Angle -> Double -> Double
paramLine x θ = \t -> (t - x) * tan θ


---- 
type Direction = String
type SegmentLength = Double
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

css :: XCoord -> Angle -> IO ()
css x t = do
putStr "xs at y crossings:\n(x val, y)\n"
putStr.unlines.(take 7).(map show) $ ycrossings x t
putStr "\nys at x crossings:\n(x, y val, pn+1 - pn)\n"
putStr.unlines.(take 7).(map show) $ xcrossings x t
