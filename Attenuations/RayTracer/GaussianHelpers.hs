module RayTracer.GaussianHelpers where
import RayTracer.GaussianBeam
-- import Data.Random.Normal
import System.Random

cheapBeam = do
  let vals = take 20 $ beam 1
  putStr "(x, z)  (θ, φ)\n"
  putStr.unlines.(map show) $ vals

cheapAngles = do
  cheapθs
  cheapφs

cheapθs = do
  let ins = [(0,1,1),(0,1,0),(0,-1,1),(0,-1,0),(1,0,1),
             (1,0,0),(-1,0,1),(-1,0,0)]
  let vals = [(x, z, d, theta x z d) | (x,z,d)<-ins]
  putStr "X Z D θ\n"
  putStr.unlines.(map show) $ vals
  putStr "\n"

cheapφs = do
  let ins = [(0,1,1),(0,1,0),(0,-1,1),(0,-1,0),(1,0,1),
             (1,0,0),(-1,0,1),(-1,0,0)]
  let vals = [(x, z, d, phi x z d) | (x,z,d)<-ins]
  putStr "X Z D φ\n"
  putStr.unlines.(map show) $ vals
  putStr "\n"


{-- some kind of dependent theory

  theta x z d =
    let x' = if x == 0 then 10**(-13) else x in
    pi/2 - atan (norm x z / d) * cos (atan (z/x'))

  phi x z d =
    let x' = if x == 0 then 10**(-10) else x in
    atan (norm x z / d) * sin (atan (z/x'))
--}

-- test theta theta'
test f g = and [cond f g x z d| (x,z,d) <- take 100 randos3]
  where
    cond f g x z d = abs (f x z d - g x z d) < 10**(-10)

randos3 = [ (x, z, d) | (d, (x, z)) <- zip randos10 $ zip randosI randosJ]
  where
    randosJ  = randomRs (0, 1::Double) $ mkStdGen 11
    randosI  = randomRs (0, 1::Double) $ mkStdGen 16
    randos10 = randomRs (0,10::Double) $ mkStdGen 23