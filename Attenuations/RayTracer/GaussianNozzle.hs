module RayTracer.GaussianNozzle where

type Perspective = (EntryCoords, EntryAngles)
type EntryCoords = (XCoord, ZCoord)
type EntryAngles = (Angle, Angle)
type Distance = Double
type XCoord = Double
type YCoord = Double
type ZCoord = Double
type Angle  = Double

nozzle :: Perspective -> Distance -> [Perspective]
nozzle ((x, z), (θ, φ)) d = [((x, z), (θ, φ))]

