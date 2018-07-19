module RayTracer.GaussianHelpers where
import RayTracer.GaussianBeam
import System.Random

-- verify that gaussian has ~ equal parts about 50
radToDeg :: Double -> Double
radToDeg θ = θ * 180 / pi

-- centered at (c, c) with distance d
-- cheapBeam 50 50
cheapBeam d c = do
  let vals = take 10 $ f (beam d c)
  let s2 = sqrt 2 / 2
  let dd = c * 2.0
  putStr "(x ,z ,θ ,φ ) in degrees\n"

  let inits = f $ map (ray d 0) [(c,c), (dd,c), (c,dd), (dd*s2,dd*s2)]
  putStr.unlines.(map show) $ inits
  putStr "\n"
  let inits = f $ map (ray d 0) [(-c,c), (-dd,c), (-c,dd), (-dd*s2,dd*s2)]
  putStr.unlines.(map show) $ inits
  putStr "\n"
  let inits = f $ map (ray d 0) [(-c,-c), (-dd,-c), (-c,-dd), (-dd*s2,-dd*s2)]
  putStr.unlines.(map show) $ inits
  putStr "\n"
  -- putStr.unlines.(map show) $ vals
  where
    f rs = [(round x, round z, g θ, g φ) | ((x,z),(θ,φ)) <- rs]
    g ω = round.radToDeg $ ω

cheapAngles = do
  let ins = [(0,1,1),(0,1,0),(0,-1,1),(0,-1,0),(1,0,1),
             (1,0,0),(-1,0,1),(-1,0,0),(sqrt 2/2, sqrt 2/2, 1),
             (sqrt 2/2, -sqrt 2/2, 1)]
  putStr "X Z D θ\n"
  cheapie theta ins
  putStr "X Z D φ\n"
  cheapie phi ins

  where
    f d = if d == 0 then 10**(-13) else d
    phi x z d = pi/2 - atan (z/f d)
    theta x z d = atan (x/f d)

    cheapie aa ins = do
      let vals = [(x, z, d, aa x z d) | (x,z,d) <- ins]
      putStr.unlines.(map show) $ vals
      putStr "\n"
