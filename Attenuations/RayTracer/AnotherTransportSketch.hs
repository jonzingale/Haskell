module RayTracer.AnotherTransportSketch where
import RayTracer.TransportHelpers
import RayTracer.RegionDetection
import RayTracer.FileToVector
import RayTracer.RayLength

type XCoord = Double
type YCoord = Double

-- xcrossings are dependent on either θ < π/2 or θ > π/2
xcrossings :: XCoord -> Angle -> [YCoord]
xcrossings x theta
  | theta > pi/2 = [-(k + frac x)*tan theta | k<-[0..]]
  | otherwise = [(1 - frac x + k)*tan theta | k<-[0..]]
  | otherwise = []
  where frac = snd.properFraction

-- ycrossings are dependent on either θ < π/2 or θ > π/2
ycrossings :: XCoord -> Angle -> [XCoord]
ycrossings x theta
  | theta > pi/2 = [-(k + frac x)*tan theta | k<-[0..]]
  | otherwise = [(1 - frac x + k)*tan theta | k<-[0..]]
  | otherwise = []
  where frac = snd.properFraction

