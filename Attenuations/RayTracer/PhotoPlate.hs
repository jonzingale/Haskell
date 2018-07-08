module RayTracer.PhotoPlate where

import qualified Data.Vector.Unboxed as U
import RayTracer.Transport
import RayTracer.Crossings

type ExitCoords = (XCoord, ZCoord)
type Attenuation = Double
type Distance = Double

{--
Here there should be a method for averaging
the rays and returning a UArray to publish
as a File.

The exit coords can be precalculated separately
from the transport and so probably should.

Of course, the fidelity may not need be any better
than 1000 x 1000 and so the x-z projection on the
array value may be sufficient.

I will try the first one.
--}

-- exitCoords (25, 0.2) (pi/4, pi/3)

-- scale -> transport -> scale
exitCoords :: EntryCoords -> EntryAngles -> ExitCoords
exitCoords (x, z) as =
  let cs = (x / 1000, z / 1000) in
  let ((i, j, _), _) = head $ exitVals cs as in
  (i*1000, j*1000)

exitVals:: EntryCoords-> EntryAngles -> [(Coords, SegmentLength)]
exitVals (x, z) (θ, φ) =
    f (xcrossings (x, z) (θ, φ))
      (ycrossings (x, z) (θ, φ))
      (zcrossings (x, z) (θ, φ))
      (x, 0, z) -- pt
      (floor x, 0, floor z) -- i j k, z offset by φ?
      (orient θ, orient φ) -- sig
  where
    orient τ | τ > pi/2 = -1
             | otherwise = 1

    -- xcs ycs zcs (p,q,r) (i,j,k) sig
    f ((xh,yh,zh): xcs) ((xv,yv,zv): ycs) ((xd,yd,zd): zcs) pt (i, j, k) (sθ, sφ)
      | yh == minimum [yh, yv, yd] = -- x case
        ((xh,yh,zh), segment pt (xh,yh,zh)) :
          f xcs ((xv,yv,zv): ycs) ((xd,yd,zd): zcs)
          (xh,yh,zh) (i+sθ, j, k) (sθ, sφ)

      | yd == minimum [yh, yv, yd] = -- z case
        (((xd,yd,zd)), segment pt (xd,yd,zd)) :
          f ((xh,yh,zh): xcs) ((xv,yv,zv): ycs) zcs
          (xd,yd,zd) (i, j, k+sφ) (sθ, sφ)

      | yv == minimum [yh, yv, yd] = -- y case
        ((xv,yv,zv), segment pt (xv,yv,zv)) :
          f ((xh,yh,zh): xcs) ycs ((xd,yd,zd): zcs)
            (xv,yv,zv) (i, j+1, k) (sθ, sφ)
