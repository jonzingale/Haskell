module RayTracer.GaussianNozzle where

type Perspective = (EntryCoords, EntryAngles)
type EntryCoords = (XCoord, ZCoord)
type EntryAngles = (Angle, Angle)
type Distance = Double
type XCoord = Double
type YCoord = Double
type ZCoord = Double
type Angle  = Double

{--
Perhaps build a file contain data
for a conic projection.
Perhaps write a random number generator
using system data and producing a gaussian process.

Then there likely should be an affine or projective
transformation passing the conic to the interface.
--}

nozzle :: Perspective -> Distance -> [Perspective]
nozzle ((x, z), (θ, φ)) d = [((x, z), (θ, φ))]

