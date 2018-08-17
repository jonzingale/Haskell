module RayTracer.GaussianHelpers where
import RayTracer.GaussianBeam
import RayTracer.Constants
import System.Random

radToDeg :: Double -> Double
radToDeg θ = θ * 180 / pi

cheapTrans = do
  let string = ["0   => ","1   => ","2   => ", "Big => "]
  let them = [s ++ f (ray k (1,0)) | (s, k)<- zip string [0, 1, 2, 10^10]]
  putStr.unlines $ them

  putStr "\nNegative xs\n"
  let string = ["0   => ","1   => ","2   => ", "Big => "]
  let them = [s ++ f (ray k (-1,0)) | (s, k)<- zip string [0, 1, 2, 10^10]]
  putStr.unlines $ them

  where
    f ((x,_),(θ,_)) = "x: " ++ (show.round )x ++ ", θ: " ++ show θ

-- centered at (c, c) with distance d
cheapBeam d s= do
  let c = center
  let vals = take 10 $ f (beam d 1 s)
  let s2 = sqrt 2 / 2
  let dd = c * 2.0
  putStr "(x ,z ,θ ,φ ) in degrees\n"

  let inits = f $ map (ray d) [(c,c), (dd,c), (c,dd), (dd*s2,dd*s2)]
  putStr.unlines.(map show) $ inits
  putStr "\n"
  let inits = f $ map (ray d) [(-c,c), (-dd,c), (-c,dd), (-dd*s2,dd*s2)]
  putStr.unlines.(map show) $ inits
  putStr "\n"
  let inits = f $ map (ray d) [(-c,-c), (-dd,-c), (-c,-dd), (-dd*s2,-dd*s2)]
  putStr.unlines.(map show) $ inits
  putStr "\n"
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
