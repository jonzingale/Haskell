module Tournament where
import HavelHakimi
-- :set +s

{--
The density creates a list of vertex multiplicities from a given
collection of edges. It then procedes to calculate the mean
and standard deviation. Lastly, it gives the proportion of
multiplicities within one standard deviation of the mean.
This percentage is about 1/2 for most Havel-Hakimi graphs,
Indicating that most competitors stand about as good a
chance of increasing their elo score over the tournament
as any other competitor.
--}

type Edge = (Int, Int)
type Vertex = Int
type Degree = Int

examples = [(n, density $ tournament n) | n <-[8..], even n]

density :: [Edge] -> Float
density edges = let datums = map snd $ counts edges in
                let std = floor $ standard_dev edges in
                let mu = foldr (+) 0 datums `div` (length datums) in
                let cond n = abs(mu - n) < std in
  (f.filter cond $ datums) / (f datums)
  where
    f = fromIntegral.length

standard_dev :: [Edge] -> Float -- ~ 4 for 10 verts.
standard_dev pair_list = sqrt.fromIntegral.pair_to_var $ pair_list

pair_to_var :: [Edge] -> Int
pair_to_var edges = let vals = map snd $ counts edges in
  avg [((avg vals) - x)^2 | x <- vals]
  where
    avg edges = foldr (+) 0 edges `div` (length edges)

-- low to high
counts :: [Edge] -> [(Vertex, Int)]
counts [] = []
counts edges = let biggie = maximum.flatten $ edges in
  [(n, countem n edges) | n<-[1..biggie]]
  where
    flatten [] = []
    flatten ((u,v):xs) = u : v : flatten xs
    countem n [] = 0
    countem n ((u,v):xs) | u == n = v + countem n xs
                         | v == n = u + countem n xs
                         | otherwise = countem n xs

list :: [Edge]
list = [(10,9),(10,8),(10,7),(10,6),(10,5),(10,4),(10,3),(2,1),(2,9),(2,8),
        (2,7),(2,6),(2,5),(2,4),(3,1),(3,9),(3,8),(3,7),(3,6),(3,5),(4,1),
        (4,9),(4,8),(4,7),(4,6),(5,1),(5,9),(5,8),(5,7),(6,1),(6,9),(6,8),
        (7,1),(7,9),(8,1)]
