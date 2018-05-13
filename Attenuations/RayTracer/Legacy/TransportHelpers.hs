module RayTracer.TransportHelpers where
import RayTracer.RayLength

negEx = [-k * tan (4*pi/5)| k<-[0..(fromIntegral.floor) 6]]
posEx = [(k-6) * tan (pi/5)| k<-[(fromIntegral.ceiling) 6..(9/tan (pi/5) + 6)]]
posEx' = [k * tan (pi/5)| k<-[0..(9/tan (pi/5))]]

paramLine :: XPoint -> Angle -> Double -> Double
paramLine x θ = \t -> (t - x) * tan θ
