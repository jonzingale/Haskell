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
    λ = fromIntegral maxVal / maxLorax
    t x = fromIntegral.floor $ λ * x
    -- does this do what i think it does?
    prj (x,y,_) = div (t x) 2 + div (t y) 2

singleLorenz = makeWavFile lorenzSingle
doubleLorenz = makeWavFile lorenzPair

