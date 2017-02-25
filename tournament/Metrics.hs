module Metrics where
import Tournament

-- :set +s

t1, t2, t3, t4, t5 :: Graph -- tournaments under different seeds.
t1 = [(8,7),(8,6),(8,5),(4,3),(4,2),(4,1),(7,6),(7,5),(3,2),(3,1),(6,5),(2,1)]
t2 = [(2,4),(2,7),(2,6),(5,3),(5,1),(5,8),(8,7),(8,1),(3,4),(3,6),(6,4),(7,1)]
t3 = [(2,6),(2,8),(2,3),(5,4),(5,1),(5,7),(4,7),(4,1),(8,6),(8,3),(6,3),(7,1)]
t4 = [(3,2),(3,7),(3,5),(8,6),(8,1),(8,4),(5,4),(5,2),(7,1),(7,6),(6,4),(1,2)]
t5 = [(8,2),(8,7),(8,6),(5,3),(5,4),(5,1),(4,2),(4,3),(7,6),(7,1),(6,2),(1,3)]

{--
  The idea at the moment is that by calculating the variance
  about each vertex and the mean about each vertex, I can
  get a sense for how much each player stands to gain in a
  given tournament graph. Finding a graph where the variance
  of this variance is minimum gives the graph which is most
  'fair' to each player. So far in the 8 competitor situation,
  the most fair is a disjoint graph of two tetrahedrons, t1 above.

  Sarah suggests that the two metrics which
  matter matter most are: 
  1) the variance of the variance about each node as
     a way to minimize the difference in ranking
     spread each competitor will face.
  2) how well centered the average rank is
     about the rank of each competitor.
     (8, [7,6,5]) is better than (8, [1,2,3]).
     min.max $ [it-mu(it)].
--}
-- Stats: starting from known graphs
best_var :: [Graph] -> Graph -- gives t1 for the above.
best_var [] = []
best_var (graph:[]) = graph
best_var (g:h:hs) | (var.vares) g < (var.vares) h = best_var (g:hs)
                  | otherwise  = best_var (h:hs)

graphs = [t1, t2, t3, t4, t5]
stats :: Graph -> (Int, Int) -- avg.avg var.var
stats graph = (avg.muse $ graph, var.vares $ graph)

all_stats :: [Graph] -> [(Int, Int)]
all_stats graphs = map stats graphs 

dev :: [Int] -> Float
dev vs = sqrt.fromIntegral.var $ vs

avg :: [Int] -> Int
avg xs = foldr (+) 0 xs `div` length xs

var :: [Int] -> Int
var vs = avg [(v - (avg vs))^2 | v <- vs]

--- About each Vertex
muse :: Graph -> [Int]
muse es = map (avg.snd) $ s_and_ts es 

vares :: Graph -> [Int]
vares es = map (var.snd) $ s_and_ts es 

s_and_ts :: Graph -> [(Vertex, [Int])]
s_and_ts [] = []
s_and_ts edges = let biggie = maximum.flatten $ edges in
  [(n, countem n edges) | n<-[1..biggie]]
  where
    flatten [] = []
    flatten ((u,v):xs) = u : v : flatten xs
    countem n [] = []
    countem n ((u,v):xs) | u == n = v : countem n xs
                         | v == n = u : countem n xs
                         | otherwise = countem n xs

--- About the graph
standard_dev :: Graph -> Float -- ~ 4 for 10 verts.
standard_dev pair_list = sqrt.fromIntegral.pair_to_var $ pair_list

pair_to_var :: Graph -> Int
pair_to_var edges = var.map snd $ [(i, sum j)| (i,j) <- s_and_ts edges]

