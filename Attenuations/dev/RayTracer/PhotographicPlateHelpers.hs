-- http://hackage.haskell.org/package/diffarray-0.1.1/docs/Data-Array-Diff.html

{-# LANGUAGE BangPatterns #-}

module RayTracer.PhotographicPlateHelpers where
import RayTracer.PhotographicPlate
import RayTracer.FileToVector
import RayTracer.Transport

import System.Random (randoms, mkStdGen) 

testRay :: Ray
testRay = ((50.0, 50.0),(pi/4, pi/2))

mean :: [Double] -> Double
mean xs = sum xs / (fromIntegral.length $ xs)

randos :: [Double] -- (0, 1)
randos = randoms.mkStdGen $ 32
them = take (10^5) $ randos -- mean: 0.49972620077629276

testUpdate = do
  ary <- fileToAry "./Tests/data1M" -- :: U.Vector Double
  putStr.show $ qArray2D 1000 (0,0) ary
  let bry = uArray2D 1000 (0,0) 1.0 ary
  putStr $ "\n" ++ show (qArray2D 1000 (0,0) bry) ++ "\n"

testRayToPlate = do
  ary <- fileToAry "./Tests/data1M" -- :: U.Vector Double
  let ray = ((1,2),(3,4))
  let plate = rayToPlate ray ary
  putStr.show $ qArray2D 1000 (0,0) ary

attenuationTest ((x, z), (θ, φ)) = do -- use laziness to break if filter.
  ary <- fileToAry "./Tests/data1M" -- :: U.Vector Double
  let path = takeWhile stopCond $ transport (x, z) (θ, φ)
  let s = sum [ seg * qArray 100 ijk ary | (ijk, seg) <- path]
  let (i,j,k) = fst.last $ path
  return $ (i, k, s)
  where
    stopCond ((x,y,z), s) =
      x<100 && y<100 && z<100 &&
      x>=0  && y>=0  && z>=0

