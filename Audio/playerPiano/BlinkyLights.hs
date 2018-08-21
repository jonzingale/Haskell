module BlinkyLights where
import qualified Data.Vector.Unboxed as U
import System.Random

type Board = U.Vector Double
type Index = Int

tenK = 1000
halfZeros = take tenK $ repeat 0
initState = U.fromList $ [1::Double] ++ halfZeros

randos :: Board
randos = U.fromList $ map fromIntegral $ take tenK $
            randomRs(0,1::Int) $ mkStdGen 23

toBin :: Int -> [Double]
toBin n = take 8 $ (f n) ++ repeat 0.0
  where
    f 0 = []
    f n = (fromIntegral.mod n) 2 : (f.div n) 2

rule neigh =
  let [a,b,c,d,e,f,g,h] = toBin 90 in
  case neigh of
    [0,0,0] -> a
    [0,0,1] -> b
    [0,1,0] -> c
    [0,1,1] -> d
    [1,0,0] -> e
    [1,0,1] -> f
    [1,1,0] -> g
    [1,1,1] -> h

neighbors :: Board -> Index -> [Double]
neighbors b i = map ((U.!) b)[mod (i-1) tenK, i, mod (i+1) tenK]

update :: Board -> Board
update b = (U.//) b [(i, f b i) | i<-[0..tenK-1]]
  where f b = rule.neighbors b

display = do
  let dynamics = iterate update initState
  putStr.unlines $ map (show.(U.toList)) $ dynamics