module LorenzWave where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Lorenz
import Wave

-- This module makes wave files from lorenz trajectories
-- single creates file from x coordinate, double from x and y

time = 20 -- size 882000
maxVal = (2^31-1) :: Int
maxLorax = 20 :: Double
maxLoray = 28 :: Double
maxLoraz = 48 :: Double

lorenzSingle :: U.Vector Int32
lorenzSingle =
  U.map prj $ U.fromList $ runLorenz time (1,1,1)
  where
    λ = fromIntegral maxVal / maxLorax
    prj (x,_,_) = fromIntegral.floor $ λ * x

lorenzPair :: U.Vector Int32
lorenzPair =
  U.map prj $ U.fromList $ runLorenz time (1,1,1)
  where
    λ = fromIntegral maxVal / maxLoraz
    t x = fromIntegral.floor $ λ * x
    -- prj (x,y,z) = t y
    prj (x,y,_) = div (t x) 2 + div (t y) 2
    -- prj (x,y,z) = div (t x) 3 + div (t y) 3 + div (t z) 3

singleLorenz = makeWavFile lorenzSingle
doubleLorenz = makeWavFile lorenzPair

