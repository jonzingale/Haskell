module RayTracer.PhotoPlate where

type ExitCoords = (XCoord, ZCoord)
type Attenuation = Double
type Distance = Double
type XCoord = Double
type YCoord = Double
type ZCoord = Double
type Angle  = Double

{--
Here there should be a method for averaging
the rays and returning a UArray to publish
as a File.
--}

plate :: Perspective -> Distance -> [(ExitCoords, Attenuation)]
nozzle ((x, z), (θ, φ)) d = [((x, z), (θ, φ))]
