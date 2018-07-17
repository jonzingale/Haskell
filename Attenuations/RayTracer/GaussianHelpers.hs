module RayTracer.GaussianHelpers where
import RayTracer.GaussianBeam
import System.Random

-- verify that gaussian has ~ equal parts about 50
radToDeg :: Double -> Double
radToDeg θ = θ * 180 / pi

-- centered at (x, z) with distance d
cheapBeam d (x, z) = do
  let vals = take 20 $ f (beam d (x, z))
  let s2 = sqrt 2 / 2
  let inits = f $ map (ray d) [(50,50), (100,50),
                               (50,100),(100*s2,100*s2)]
  putStr "(x ,z , θ deg, φ deg)\n"
  putStr.unlines.(map show) $ inits
  putStr "\n" 
  putStr.unlines.(map show) $ vals
  where
    f rs = [(round x, round z, g θ, g φ) | ((x,z),(θ,φ)) <- rs]
    e t x = abs (x-t) < 10**(-10)
    rr x = (fromIntegral.round $ x*10^2)/10^2::Double
    g ω = rr.radToDeg $ ω

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
