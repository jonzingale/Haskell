module RayTracer.Transport3D where
import RayTracer.HelpersTransport3D
import RayTracer.Crossings

type SegmentLength = Double

{--
This is going to need very very much work.
θ, φ cases individually.
--}
type IntCoords = (Int, Int, Int)
transport:: EntryCoords-> EntryAngles -> [(IntCoords, SegmentLength)]
transport (x, z) (θ, φ)
  | θ > pi/2 && φ > pi/2 =
                 f (xcs (x, z) (θ, φ))
                 (ycs (x, z) (θ, φ))
                 (zcs (x, z) (θ, φ))
                 (x, 0, z) -- pt
                 (floor x, 0, floor z) -- i j k, z offset by φ?
                 (negate 1, negate 1) -- sig
  | θ > pi/2 && φ <= pi/2 =
                 f (xcs (x, z) (θ, φ))
                 (ycs (x, z) (θ, φ))
                 (zcs (x, z) (θ, φ))
                 (x, 0, z) -- pt
                 (floor x, 0, floor z) -- i j k, z offset by φ?
                 (negate 1, 1) -- sig
  | θ <= pi/2 && φ <= pi/2 =
                 f (xcs (x, z) (θ, φ))
                 (ycs (x, z) (θ, φ))
                 (zcs (x, z) (θ, φ))
                 (x, 0, z)
                 (floor x, 0, floor z)
                 (1, 1)
  | θ <= pi/2 && φ > pi/2 =
                 f (xcs (x, z) (θ, φ))
                 (ycs (x, z) (θ, φ))
                 (zcs (x, z) (θ, φ))
                 (x, 0, z) -- pt
                 (floor x, 0, floor z)
                 (1, negate 1)
  where
    (xcs, ycs, zcs) = (xcrossings, ycrossings, zcrossings)

    -- order of the following methods matters.
    -- xcs ycs zcs (p,q,r) (i,j,k) sig
    f ((xh,yh,zh): xcs) ((xv,yv,zv): ycs) ((xd,yd,zd): zcs) pt (i, j, k) (sθ, sφ)
      | yh == minimum [yh, yv, yd] = -- x case
        ((i,j,k), segment pt (xh,yh,zh)) :
          f xcs ((xv,yv,zv): ycs) ((xd,yd,zd): zcs)
          (xh,yh,zh) -- pt
          (i+sθ, j, k)
          (sθ, sφ)

      | yd == minimum [yh, yv, yd]  = -- z case
        ((i,j,k), segment pt (xd,yd,zd)) :
          f ((xh,yh,zh): xcs) ((xv,yv,zv): ycs) zcs
          (xd,yd,zd) -- pt
          (i, j, k+sφ)
          (sθ, sφ)

      | yv == minimum [yh, yv, yd] = -- y case
        ((i,j,k), segment pt (xv,yv,zv)) :
          f ((xh,yh,zh): xcs) ycs ((xd,yd,zd): zcs)
            (xv,yv,zv) -- pt
            (i, j+1, k)
            (sθ, sφ)

segment :: Coords -> Coords -> SegmentLength
segment (x1, y1, z1) (x2, y2, z2) =
  sqrt $ (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2