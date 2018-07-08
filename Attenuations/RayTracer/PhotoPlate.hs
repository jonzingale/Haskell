module RayTracer.PhotoPlate where
import qualified Data.Vector.Unboxed as U

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

plate :: [ExitCoords] -> Distance -> [(ExitCoords, Attenuation)]
plate cs d = [(c, 1.0) | c <- cs ]

