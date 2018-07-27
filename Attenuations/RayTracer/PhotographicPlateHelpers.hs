-- http://hackage.haskell.org/package/diffarray-0.1.1/docs/Data-Array-Diff.html

{-# LANGUAGE BangPatterns #-}

module RayTracer.PhotographicPlateHelpers where
import RayTracer.PhotographicPlate
import RayTracer.FileToVector

import System.Random (randoms, mkStdGen) 

randos :: [Double] -- (0, 1)
randos = randoms.mkStdGen $ 32
them = take (10^5) $ randos -- mean: 0.49972620077629276

testUpdate = do
  ary <- fileToAry "./Tests/data1M" -- :: U.Vector Double
  putStr.show $ qArray2D 1000 (0,0) ary
  let bry = uArray2D 1000 (0,0) 1.0 ary
  putStr $ "\n" ++ show (qArray2D 1000 (0,0) bry) ++ "\n"

mean :: [Double] -> Double
mean xs = sum xs / (fromIntegral.length $ xs)