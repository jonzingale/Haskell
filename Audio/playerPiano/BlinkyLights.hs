module BlinkyLights where
import qualified Data.Vector.Unboxed as U

type Board = U.Vector Double
type Index = Int

tenK = 10^2
halfZeros = take tenK $ repeat 0
initState = U.fromList $ [1::Double] ++ halfZeros

rule neigh = case neigh of
  [0,0,0] -> 0
  [0,0,1] -> 1
  [0,1,0] -> 0
  [0,1,1] -> 1
  [1,0,0] -> 1
  [1,0,1] -> 1
  [1,1,0] -> 1
  [1,1,1] -> 0

neighbors :: Board -> Index -> [Double]
neighbors b i = map ((U.!) b)[mod (i-1) tenK, i, mod (i+1) tenK]

update :: Board -> Board
update b = (U.//) b [(i, f b i) | i<-[0..tenK-1]]
  where f b = rule.neighbors b

display = do
  let dynamics = iterate update initState
  putStr.unlines $ map (show.(U.toList)) $ dynamics