module RayTracer.AnotherTransportSketch where
import RayTracer.TransportHelpers
import RayTracer.RegionDetection
import RayTracer.FileToVector
import RayTracer.RayLength

type XCoord = Double
type YCoord = Double

css x t = do
  putStr "ys at x crossings:\n(x, y val)\n"
  putStr.unlines.(map show) $ xcrossings x t
  putStr "\nxs at y crossings:\n(x val, y)\n"
  putStr.unlines.(map show) $ ycrossings x t

-- xcrossings are dependent on either θ < π/2 or θ > π/2.
-- These y values should always go positive. There likely
-- hides a symmetry about pi/2.
xcrossings :: XCoord -> Angle -> [(XCoord, YCoord)]
xcrossings x theta
  | theta > pi/2 = take 5 [(cc x - k - 1, -(k + frac x)*tan theta) | k<-[0..]]
  | theta < pi/2 = take 5 [(ff x + k + 1, (1 - frac x + k)*tan theta) | k<-[0..]]
  | otherwise = []
  where frac = snd.properFraction

-- ycrossings are dependent on either θ < π/2 or θ > π/2.
-- These x values may go negative. All three cases the same!
ycrossings :: XCoord -> Angle -> [(XCoord, YCoord)]
ycrossings x theta = take 5 [ (x + k / tan theta, k) | k <- [0..]]

cc, ff :: Double -> Double
cc = fromIntegral.ceiling
ff = fromIntegral.floor