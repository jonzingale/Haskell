module DynDistr where
{--
(connections, dyns count)
[(2,2),(4,7),(5,4),(6,8),(8,5),(10,24),(12,14),(13,8),
 (15,10),(16,28),(18,2),(20,4),(21,6),(24,16),(25,20),(26,36),
 (29,2),(30,10),(31,2),(32,13),(34,18),(36,4),(39,6),(40,38),
 (42,14),(49,3),(52,4),(55,27),(58,8),(60,18),(62,8),(64,15),
 (65,20),(68,12),(70,2),(75,2),(78,4),(81,6),(83,4),(84,10),
 (87,2),(89,18),(96,6),(98,6),(100,6),(119,2),(121,6),(124,4),
 (127,2),(131,14),(134,8),(136,4),(140,6),(144,7),(145,6),(169,2),
 (181,2),(189,4),(196,4),(200,4),(205,4),(268,4),(288,5),(292,4),
 (408,2),(577,1)]
--}
count_distr :: [(Integer, Integer)]
count_distr = f target_distributions 1
  where
    f [] _ = []
    f ((a, b):[]) n = [(b, n)]
    f ((a, b):(c, d):es) n | d == b = f ((c, d):es) (n+1)
                           | otherwise = (b, n) : f ((c, d):es) 1

graph = [[0,0,0,0,0,0,1],[0,0,0,0,0,0,2],[0,0,0,0,0,1,0],[0,0,0,0,0,1,1],[0,0,0,0,0,2,0],[0,0,0,0,0,2,2],[0,0,0,0,1,0,0],[0,0,0,0,1,0,1],[0,0,0,0,1,0,2],[0,0,0,0,1,1,0],[0,0,0,0,1,1,1],[0,0,0,0,2,0,0],[0,0,0,0,2,0,1],[0,0,0,0,2,0,2],[0,0,0,0,2,2,0],[0,0,0,0,2,2,2],[0,0,0,1,0,0,0],[0,0,0,1,0,0,1],[0,0,0,1,0,0,2],[0,0,0,1,0,1,0],[0,0,0,1,0,1,1],[0,0,0,1,0,2,0],[0,0,0,1,0,2,2],[0,0,0,1,1,0,0],[0,0,0,1,1,0,1],[0,0,0,1,1,0,2],[0,0,0,1,1,1,0],[0,0,0,1,1,1,1],[0,0,0,2,0,0,0],[0,0,0,2,0,0,1],[0,0,0,2,0,0,2],[0,0,0,2,0,1,0],[0,0,0,2,0,1,1],[0,0,0,2,0,2,0],[0,0,0,2,0,2,2],[0,0,0,2,2,0,0],[0,0,0,2,2,0,1],[0,0,0,2,2,0,2],[0,0,0,2,2,2,0],[0,0,0,2,2,2,2],[0,0,1,0,0,0,0],[0,0,1,0,0,0,1],[0,0,1,0,0,0,2],[0,0,1,0,0,1,0],[0,0,1,0,0,1,1],[0,0,1,0,0,2,0],[0,0,1,0,0,2,2],[0,0,1,0,1,0,0],[0,0,1,0,1,0,1],[0,0,1,0,1,0,2],[0,0,1,0,1,1,0],[0,0,1,0,1,1,1],[0,0,1,0,2,0,0],[0,0,1,0,2,0,1],[0,0,1,0,2,0,2],[0,0,1,0,2,2,0],[0,0,1,0,2,2,2],[0,0,1,1,0,0,0],[0,0,1,1,0,0,1],[0,0,1,1,0,0,2],[0,0,1,1,0,1,0],[0,0,1,1,0,1,1],[0,0,1,1,0,2,0],[0,0,1,1,0,2,2],[0,0,1,1,1,0,0],[0,0,1,1,1,0,1],[0,0,1,1,1,0,2],[0,0,1,1,1,1,0],[0,0,1,1,1,1,1],[0,0,2,0,0,0,0],[0,0,2,0,0,0,1],[0,0,2,0,0,0,2],[0,0,2,0,0,1,0],[0,0,2,0,0,1,1],[0,0,2,0,0,2,0],[0,0,2,0,0,2,2],[0,0,2,0,1,0,0],[0,0,2,0,1,0,1],[0,0,2,0,1,0,2],[0,0,2,0,1,1,0],[0,0,2,0,1,1,1],[0,0,2,0,2,0,0],[0,0,2,0,2,0,1],[0,0,2,0,2,0,2],[0,0,2,0,2,2,0],[0,0,2,0,2,2,2],[0,0,2,2,0,0,0],[0,0,2,2,0,0,1],[0,0,2,2,0,0,2],[0,0,2,2,0,1,0],[0,0,2,2,0,1,1],[0,0,2,2,0,2,0],[0,0,2,2,0,2,2],[0,0,2,2,2,0,0],[0,0,2,2,2,0,1],[0,0,2,2,2,0,2],[0,0,2,2,2,2,0],[0,0,2,2,2,2,2],[0,1,0,0,0,0,0],[0,1,0,0,0,0,1],[0,1,0,0,0,0,2],[0,1,0,0,0,1,0],[0,1,0,0,0,1,1],[0,1,0,0,0,2,0],[0,1,0,0,0,2,2],[0,1,0,0,1,0,0],[0,1,0,0,1,0,1],[0,1,0,0,1,0,2],[0,1,0,0,1,1,0],[0,1,0,0,1,1,1],[0,1,0,0,2,0,0],[0,1,0,0,2,0,1],[0,1,0,0,2,0,2],[0,1,0,0,2,2,0],[0,1,0,0,2,2,2],[0,1,0,1,0,0,0],[0,1,0,1,0,0,1],[0,1,0,1,0,0,2],[0,1,0,1,0,1,0],[0,1,0,1,0,1,1],[0,1,0,1,0,2,0],[0,1,0,1,0,2,2],[0,1,0,1,1,0,0],[0,1,0,1,1,0,1],[0,1,0,1,1,0,2],[0,1,0,1,1,1,0],[0,1,0,1,1,1,1],[0,1,0,2,0,0,0],[0,1,0,2,0,0,1],[0,1,0,2,0,0,2],[0,1,0,2,0,1,0],[0,1,0,2,0,1,1],[0,1,0,2,0,2,0],[0,1,0,2,0,2,2],[0,1,0,2,2,0,0],[0,1,0,2,2,0,1],[0,1,0,2,2,0,2],[0,1,0,2,2,2,0],[0,1,0,2,2,2,2],[0,1,1,0,0,0,0],[0,1,1,0,0,0,1],[0,1,1,0,0,0,2],[0,1,1,0,0,1,0],[0,1,1,0,0,1,1],[0,1,1,0,0,2,0],[0,1,1,0,0,2,2],[0,1,1,0,1,0,0],[0,1,1,0,1,0,1],[0,1,1,0,1,0,2],[0,1,1,0,1,1,0],[0,1,1,0,1,1,1],[0,1,1,0,2,0,0],[0,1,1,0,2,0,1],[0,1,1,0,2,0,2],[0,1,1,0,2,2,0],[0,1,1,0,2,2,2],[0,1,1,1,0,0,0],[0,1,1,1,0,0,1],[0,1,1,1,0,0,2],[0,1,1,1,0,1,0],[0,1,1,1,0,1,1],[0,1,1,1,0,2,0],[0,1,1,1,0,2,2],[0,1,1,1,1,0,0],[0,1,1,1,1,0,1],[0,1,1,1,1,0,2],[0,1,1,1,1,1,0],[0,1,1,1,1,1,1],[0,2,0,0,0,0,0],[0,2,0,0,0,0,1],[0,2,0,0,0,0,2],[0,2,0,0,0,1,0],[0,2,0,0,0,1,1],[0,2,0,0,0,2,0],[0,2,0,0,0,2,2],[0,2,0,0,1,0,0],[0,2,0,0,1,0,1],[0,2,0,0,1,0,2],[0,2,0,0,1,1,0],[0,2,0,0,1,1,1],[0,2,0,0,2,0,0],[0,2,0,0,2,0,1],[0,2,0,0,2,0,2],[0,2,0,0,2,2,0],[0,2,0,0,2,2,2],[0,2,0,1,0,0,0],[0,2,0,1,0,0,1],[0,2,0,1,0,0,2],[0,2,0,1,0,1,0],[0,2,0,1,0,1,1],[0,2,0,1,0,2,0],[0,2,0,1,0,2,2],[0,2,0,1,1,0,0],[0,2,0,1,1,0,1],[0,2,0,1,1,0,2],[0,2,0,1,1,1,0],[0,2,0,1,1,1,1],[0,2,0,2,0,0,0],[0,2,0,2,0,0,1],[0,2,0,2,0,0,2],[0,2,0,2,0,1,0],[0,2,0,2,0,1,1],[0,2,0,2,0,2,0],[0,2,0,2,0,2,2],[0,2,0,2,2,0,0],[0,2,0,2,2,0,1],[0,2,0,2,2,0,2],[0,2,0,2,2,2,0],[0,2,0,2,2,2,2],[0,2,2,0,0,0,0],[0,2,2,0,0,0,1],[0,2,2,0,0,0,2],[0,2,2,0,0,1,0],[0,2,2,0,0,1,1],[0,2,2,0,0,2,0],[0,2,2,0,0,2,2],[0,2,2,0,1,0,0],[0,2,2,0,1,0,1],[0,2,2,0,1,0,2],[0,2,2,0,1,1,0],[0,2,2,0,1,1,1],[0,2,2,0,2,0,0],[0,2,2,0,2,0,1],[0,2,2,0,2,0,2],[0,2,2,0,2,2,0],[0,2,2,0,2,2,2],[0,2,2,2,0,0,0],[0,2,2,2,0,0,1],[0,2,2,2,0,0,2],[0,2,2,2,0,1,0],[0,2,2,2,0,1,1],[0,2,2,2,0,2,0],[0,2,2,2,0,2,2],[0,2,2,2,2,0,0],[0,2,2,2,2,0,1],[0,2,2,2,2,0,2],[0,2,2,2,2,2,0],[0,2,2,2,2,2,2],[1,0,0,0,0,0,0],[1,0,0,0,0,0,1],[1,0,0,0,0,0,2],[1,0,0,0,0,1,0],[1,0,0,0,0,1,1],[1,0,0,0,0,2,0],[1,0,0,0,0,2,2],[1,0,0,0,1,0,0],[1,0,0,0,1,0,1],[1,0,0,0,1,0,2],[1,0,0,0,1,1,0],[1,0,0,0,1,1,1],[1,0,0,0,2,0,0],[1,0,0,0,2,0,1],[1,0,0,0,2,0,2],[1,0,0,0,2,2,0],[1,0,0,0,2,2,2],[1,0,0,1,0,0,0],[1,0,0,1,0,0,1],[1,0,0,1,0,0,2],[1,0,0,1,0,1,0],[1,0,0,1,0,1,1],[1,0,0,1,0,2,0],[1,0,0,1,0,2,2],[1,0,0,1,1,0,0],[1,0,0,1,1,0,1],[1,0,0,1,1,0,2],[1,0,0,1,1,1,0],[1,0,0,1,1,1,1],[1,0,0,2,0,0,0],[1,0,0,2,0,0,1],[1,0,0,2,0,0,2],[1,0,0,2,0,1,0],[1,0,0,2,0,1,1],[1,0,0,2,0,2,0],[1,0,0,2,0,2,2],[1,0,0,2,2,0,0],[1,0,0,2,2,0,1],[1,0,0,2,2,0,2],[1,0,0,2,2,2,0],[1,0,0,2,2,2,2],[1,0,1,0,0,0,0],[1,0,1,0,0,0,1],[1,0,1,0,0,0,2],[1,0,1,0,0,1,0],[1,0,1,0,0,1,1],[1,0,1,0,0,2,0],[1,0,1,0,0,2,2],[1,0,1,0,1,0,0],[1,0,1,0,1,0,1],[1,0,1,0,1,0,2],[1,0,1,0,1,1,0],[1,0,1,0,1,1,1],[1,0,1,0,2,0,0],[1,0,1,0,2,0,1],[1,0,1,0,2,0,2],[1,0,1,0,2,2,0],[1,0,1,0,2,2,2],[1,0,1,1,0,0,0],[1,0,1,1,0,0,1],[1,0,1,1,0,0,2],[1,0,1,1,0,1,0],[1,0,1,1,0,1,1],[1,0,1,1,0,2,0],[1,0,1,1,0,2,2],[1,0,1,1,1,0,0],[1,0,1,1,1,0,1],[1,0,1,1,1,0,2],[1,0,1,1,1,1,0],[1,0,1,1,1,1,1],[1,0,2,0,0,0,0],[1,0,2,0,0,0,1],[1,0,2,0,0,0,2],[1,0,2,0,0,1,0],[1,0,2,0,0,1,1],[1,0,2,0,0,2,0],[1,0,2,0,0,2,2],[1,0,2,0,1,0,0],[1,0,2,0,1,0,1],[1,0,2,0,1,0,2],[1,0,2,0,1,1,0],[1,0,2,0,1,1,1],[1,0,2,0,2,0,0],[1,0,2,0,2,0,1],[1,0,2,0,2,0,2],[1,0,2,0,2,2,0],[1,0,2,0,2,2,2],[1,0,2,2,0,0,0],[1,0,2,2,0,0,1],[1,0,2,2,0,0,2],[1,0,2,2,0,1,0],[1,0,2,2,0,1,1],[1,0,2,2,0,2,0],[1,0,2,2,0,2,2],[1,0,2,2,2,0,0],[1,0,2,2,2,0,1],[1,0,2,2,2,0,2],[1,0,2,2,2,2,0],[1,0,2,2,2,2,2],[1,1,0,0,0,0,0],[1,1,0,0,0,0,1],[1,1,0,0,0,0,2],[1,1,0,0,0,1,0],[1,1,0,0,0,1,1],[1,1,0,0,0,2,0],[1,1,0,0,0,2,2],[1,1,0,0,1,0,0],[1,1,0,0,1,0,1],[1,1,0,0,1,0,2],[1,1,0,0,1,1,0],[1,1,0,0,1,1,1],[1,1,0,0,2,0,0],[1,1,0,0,2,0,1],[1,1,0,0,2,0,2],[1,1,0,0,2,2,0],[1,1,0,0,2,2,2],[1,1,0,1,0,0,0],[1,1,0,1,0,0,1],[1,1,0,1,0,0,2],[1,1,0,1,0,1,0],[1,1,0,1,0,1,1],[1,1,0,1,0,2,0],[1,1,0,1,0,2,2],[1,1,0,1,1,0,0],[1,1,0,1,1,0,1],[1,1,0,1,1,0,2],[1,1,0,1,1,1,0],[1,1,0,1,1,1,1],[1,1,0,2,0,0,0],[1,1,0,2,0,0,1],[1,1,0,2,0,0,2],[1,1,0,2,0,1,0],[1,1,0,2,0,1,1],[1,1,0,2,0,2,0],[1,1,0,2,0,2,2],[1,1,0,2,2,0,0],[1,1,0,2,2,0,1],[1,1,0,2,2,0,2],[1,1,0,2,2,2,0],[1,1,0,2,2,2,2],[1,1,1,0,0,0,0],[1,1,1,0,0,0,1],[1,1,1,0,0,0,2],[1,1,1,0,0,1,0],[1,1,1,0,0,1,1],[1,1,1,0,0,2,0],[1,1,1,0,0,2,2],[1,1,1,0,1,0,0],[1,1,1,0,1,0,1],[1,1,1,0,1,0,2],[1,1,1,0,1,1,0],[1,1,1,0,1,1,1],[1,1,1,0,2,0,0],[1,1,1,0,2,0,1],[1,1,1,0,2,0,2],[1,1,1,0,2,2,0],[1,1,1,0,2,2,2],[1,1,1,1,0,0,0],[1,1,1,1,0,0,1],[1,1,1,1,0,0,2],[1,1,1,1,0,1,0],[1,1,1,1,0,1,1],[1,1,1,1,0,2,0],[1,1,1,1,0,2,2],[1,1,1,1,1,0,0],[1,1,1,1,1,0,1],[1,1,1,1,1,0,2],[1,1,1,1,1,1,0],[1,1,1,1,1,1,1],[2,0,0,0,0,0,0],[2,0,0,0,0,0,1],[2,0,0,0,0,0,2],[2,0,0,0,0,1,0],[2,0,0,0,0,1,1],[2,0,0,0,0,2,0],[2,0,0,0,0,2,2],[2,0,0,0,1,0,0],[2,0,0,0,1,0,1],[2,0,0,0,1,0,2],[2,0,0,0,1,1,0],[2,0,0,0,1,1,1],[2,0,0,0,2,0,0],[2,0,0,0,2,0,1],[2,0,0,0,2,0,2],[2,0,0,0,2,2,0],[2,0,0,0,2,2,2],[2,0,0,1,0,0,0],[2,0,0,1,0,0,1],[2,0,0,1,0,0,2],[2,0,0,1,0,1,0],[2,0,0,1,0,1,1],[2,0,0,1,0,2,0],[2,0,0,1,0,2,2],[2,0,0,1,1,0,0],[2,0,0,1,1,0,1],[2,0,0,1,1,0,2],[2,0,0,1,1,1,0],[2,0,0,1,1,1,1],[2,0,0,2,0,0,0],[2,0,0,2,0,0,1],[2,0,0,2,0,0,2],[2,0,0,2,0,1,0],[2,0,0,2,0,1,1],[2,0,0,2,0,2,0],[2,0,0,2,0,2,2],[2,0,0,2,2,0,0],[2,0,0,2,2,0,1],[2,0,0,2,2,0,2],[2,0,0,2,2,2,0],[2,0,0,2,2,2,2],[2,0,1,0,0,0,0],[2,0,1,0,0,0,1],[2,0,1,0,0,0,2],[2,0,1,0,0,1,0],[2,0,1,0,0,1,1],[2,0,1,0,0,2,0],[2,0,1,0,0,2,2],[2,0,1,0,1,0,0],[2,0,1,0,1,0,1],[2,0,1,0,1,0,2],[2,0,1,0,1,1,0],[2,0,1,0,1,1,1],[2,0,1,0,2,0,0],[2,0,1,0,2,0,1],[2,0,1,0,2,0,2],[2,0,1,0,2,2,0],[2,0,1,0,2,2,2],[2,0,1,1,0,0,0],[2,0,1,1,0,0,1],[2,0,1,1,0,0,2],[2,0,1,1,0,1,0],[2,0,1,1,0,1,1],[2,0,1,1,0,2,0],[2,0,1,1,0,2,2],[2,0,1,1,1,0,0],[2,0,1,1,1,0,1],[2,0,1,1,1,0,2],[2,0,1,1,1,1,0],[2,0,1,1,1,1,1],[2,0,2,0,0,0,0],[2,0,2,0,0,0,1],[2,0,2,0,0,0,2],[2,0,2,0,0,1,0],[2,0,2,0,0,1,1],[2,0,2,0,0,2,0],[2,0,2,0,0,2,2],[2,0,2,0,1,0,0],[2,0,2,0,1,0,1],[2,0,2,0,1,0,2],[2,0,2,0,1,1,0],[2,0,2,0,1,1,1],[2,0,2,0,2,0,0],[2,0,2,0,2,0,1],[2,0,2,0,2,0,2],[2,0,2,0,2,2,0],[2,0,2,0,2,2,2],[2,0,2,2,0,0,0],[2,0,2,2,0,0,1],[2,0,2,2,0,0,2],[2,0,2,2,0,1,0],[2,0,2,2,0,1,1],[2,0,2,2,0,2,0],[2,0,2,2,0,2,2],[2,0,2,2,2,0,0],[2,0,2,2,2,0,1],[2,0,2,2,2,0,2],[2,0,2,2,2,2,0],[2,0,2,2,2,2,2],[2,2,0,0,0,0,0],[2,2,0,0,0,0,1],[2,2,0,0,0,0,2],[2,2,0,0,0,1,0],[2,2,0,0,0,1,1],[2,2,0,0,0,2,0],[2,2,0,0,0,2,2],[2,2,0,0,1,0,0],[2,2,0,0,1,0,1],[2,2,0,0,1,0,2],[2,2,0,0,1,1,0],[2,2,0,0,1,1,1],[2,2,0,0,2,0,0],[2,2,0,0,2,0,1],[2,2,0,0,2,0,2],[2,2,0,0,2,2,0],[2,2,0,0,2,2,2],[2,2,0,1,0,0,0],[2,2,0,1,0,0,1],[2,2,0,1,0,0,2],[2,2,0,1,0,1,0],[2,2,0,1,0,1,1],[2,2,0,1,0,2,0],[2,2,0,1,0,2,2],[2,2,0,1,1,0,0],[2,2,0,1,1,0,1],[2,2,0,1,1,0,2],[2,2,0,1,1,1,0],[2,2,0,1,1,1,1],[2,2,0,2,0,0,0],[2,2,0,2,0,0,1],[2,2,0,2,0,0,2],[2,2,0,2,0,1,0],[2,2,0,2,0,1,1],[2,2,0,2,0,2,0],[2,2,0,2,0,2,2],[2,2,0,2,2,0,0],[2,2,0,2,2,0,1],[2,2,0,2,2,0,2],[2,2,0,2,2,2,0],[2,2,0,2,2,2,2],[2,2,2,0,0,0,0],[2,2,2,0,0,0,1],[2,2,2,0,0,0,2],[2,2,2,0,0,1,0],[2,2,2,0,0,1,1],[2,2,2,0,0,2,0],[2,2,2,0,0,2,2],[2,2,2,0,1,0,0],[2,2,2,0,1,0,1],[2,2,2,0,1,0,2],[2,2,2,0,1,1,0],[2,2,2,0,1,1,1],[2,2,2,0,2,0,0],[2,2,2,0,2,0,1],[2,2,2,0,2,0,2],[2,2,2,0,2,2,0],[2,2,2,0,2,2,2],[2,2,2,2,0,0,0],[2,2,2,2,0,0,1],[2,2,2,2,0,0,2],[2,2,2,2,0,1,0],[2,2,2,2,0,1,1],[2,2,2,2,0,2,0],[2,2,2,2,0,2,2],[2,2,2,2,2,0,0],[2,2,2,2,2,0,1],[2,2,2,2,2,0,2],[2,2,2,2,2,2,0],[2,2,2,2,2,2,2]]

target_distributions = [
  ([2,2,2,2,2,2,2],2),
  ([1,1,1,1,1,1,1],2),
  ([1,1,1,1,1,1,0],4),
  ([1,1,1,1,1,0,2],4),
  ([1,1,1,1,0,2,2],4),
  ([1,1,1,0,2,2,2],4),
  ([1,1,0,2,2,2,2],4),
  ([1,0,2,2,2,2,2],4),
  ([0,2,2,2,2,2,2],4),
  ([2,2,2,2,2,2,0],5),
  ([2,2,2,2,2,0,2],5),
  ([1,0,1,1,1,1,1],5),
  ([0,1,1,1,1,1,1],5),
  ([2,2,2,2,0,2,2],6),
  ([2,2,2,0,2,2,2],6),
  ([2,2,0,2,2,2,2],6),
  ([2,0,2,2,2,2,2],6),
  ([1,1,1,1,1,0,1],6),
  ([1,1,1,1,0,1,1],6),
  ([1,1,1,0,1,1,1],6),
  ([1,1,0,1,1,1,1],6),
  ([2,2,2,2,2,0,1],8),
  ([2,2,2,2,0,1,1],8),
  ([2,2,2,0,1,1,1],8),
  ([2,2,0,1,1,1,1],8),
  ([2,0,1,1,1,1,1],8),
  ([1,1,1,1,1,0,0],10),
  ([1,1,1,1,0,2,0],10),
  ([1,1,1,1,0,1,0],10),
  ([1,1,1,1,0,0,2],10),
  ([1,1,1,0,2,2,0],10),
  ([1,1,1,0,2,0,2],10),
  ([1,1,0,2,2,2,0],10),
  ([1,1,0,2,2,0,2],10),
  ([1,0,2,2,2,2,0],10),
  ([1,0,2,2,2,0,2],10),
  ([1,0,1,1,1,1,0],10),
  ([1,0,1,1,1,0,2],10),
  ([1,0,1,1,0,2,2],10),
  ([1,0,1,0,2,2,2],10),
  ([1,0,0,2,2,2,2],10),
  ([0,2,2,2,2,2,0],10),
  ([0,2,2,2,2,0,2],10),
  ([0,2,0,2,2,2,2],10),
  ([0,1,1,1,1,1,0],10),
  ([0,1,1,1,1,0,2],10),
  ([0,1,1,1,0,2,2],10),
  ([0,1,1,0,2,2,2],10),
  ([0,1,0,2,2,2,2],10),
  ([0,0,2,2,2,2,2],10),
  ([2,2,2,2,2,0,0],12),
  ([1,1,1,0,1,1,0],12),
  ([1,1,1,0,1,0,2],12),
  ([1,1,1,0,0,2,2],12),
  ([1,1,0,2,0,2,2],12),
  ([1,1,0,1,1,1,0],12),
  ([1,1,0,1,1,0,2],12),
  ([1,1,0,1,0,2,2],12),
  ([1,1,0,0,2,2,2],12),
  ([1,0,2,2,0,2,2],12),
  ([1,0,2,0,2,2,2],12),
  ([0,2,2,2,0,2,2],12),
  ([0,2,2,0,2,2,2],12),
  ([0,0,1,1,1,1,1],12),
  ([2,2,2,2,0,2,0],13),
  ([2,2,2,2,0,1,0],13),
  ([2,2,2,2,0,0,2],13),
  ([2,2,2,0,2,0,2],13),
  ([1,0,1,0,1,1,1],13),
  ([1,0,0,1,1,1,1],13),
  ([0,2,0,1,1,1,1],13),
  ([0,1,0,1,1,1,1],13),
  ([2,2,2,0,2,2,0],15),
  ([2,2,0,2,2,2,0],15),
  ([2,2,0,2,2,0,2],15),
  ([2,0,2,2,2,2,0],15),
  ([2,0,2,2,2,0,2],15),
  ([1,0,1,1,1,0,1],15),
  ([1,0,1,1,0,1,1],15),
  ([0,1,1,1,1,0,1],15),
  ([0,1,1,1,0,1,1],15),
  ([0,1,1,0,1,1,1],15),
  ([2,2,2,0,1,1,0],16),
  ([2,2,2,0,1,0,2],16),
  ([2,2,2,0,0,2,2],16),
  ([2,2,0,2,0,2,2],16),
  ([2,2,0,1,1,1,0],16),
  ([2,2,0,1,1,0,2],16),
  ([2,2,0,1,0,2,2],16),
  ([2,2,0,0,2,2,2],16),
  ([2,0,2,0,2,2,2],16),
  ([2,0,1,1,1,1,0],16),
  ([2,0,1,1,1,0,2],16),
  ([2,0,1,1,0,2,2],16),
  ([2,0,1,0,2,2,2],16),
  ([2,0,0,2,2,2,2],16),
  ([1,1,1,1,0,0,1],16),
  ([1,1,1,0,2,0,1],16),
  ([1,1,1,0,1,0,1],16),
  ([1,1,1,0,0,1,1],16),
  ([1,1,0,2,2,0,1],16),
  ([1,1,0,2,0,1,1],16),
  ([1,1,0,1,0,1,1],16),
  ([1,1,0,0,1,1,1],16),
  ([1,0,2,2,2,0,1],16),
  ([1,0,2,2,0,1,1],16),
  ([1,0,2,0,1,1,1],16),
  ([0,2,2,2,2,0,1],16),
  ([0,2,2,2,0,1,1],16),
  ([0,2,2,0,1,1,1],16),
  ([2,0,2,2,0,2,2],18),
  ([1,1,0,1,1,0,1],18),
  ([2,2,2,2,0,0,1],20),
  ([2,2,2,0,0,1,1],20),
  ([2,2,0,0,1,1,1],20),
  ([2,0,0,1,1,1,1],20),
  ([2,2,2,0,2,0,1],21),
  ([2,2,2,0,1,0,1],21),
  ([2,2,0,2,0,1,1],21),
  ([2,2,0,1,0,1,1],21),
  ([2,0,2,0,1,1,1],21),
  ([2,0,1,0,1,1,1],21),
  ([2,2,0,2,2,0,1],24),
  ([2,2,0,1,1,0,1],24),
  ([2,0,2,2,2,0,1],24),
  ([2,0,2,2,0,1,1],24),
  ([2,0,1,1,1,0,1],24),
  ([2,0,1,1,0,1,1],24),
  ([1,1,1,1,0,0,0],24),
  ([1,1,1,0,2,0,0],24),
  ([1,1,0,2,2,0,0],24),
  ([1,0,2,2,2,0,0],24),
  ([0,2,2,2,2,0,0],24),
  ([0,0,1,1,1,1,0],24),
  ([0,0,1,1,1,0,2],24),
  ([0,0,1,1,0,2,2],24),
  ([0,0,1,0,2,2,2],24),
  ([0,0,0,2,2,2,2],24),
  ([1,0,1,1,1,0,0],25),
  ([1,0,1,1,0,2,0],25),
  ([1,0,1,1,0,1,0],25),
  ([1,0,1,1,0,0,2],25),
  ([1,0,1,0,2,2,0],25),
  ([1,0,1,0,2,0,2],25),
  ([1,0,0,2,2,2,0],25),
  ([1,0,0,2,2,0,2],25),
  ([0,2,0,2,2,2,0],25),
  ([0,2,0,2,2,0,2],25),
  ([0,1,1,1,1,0,0],25),
  ([0,1,1,1,0,2,0],25),
  ([0,1,1,1,0,1,0],25),
  ([0,1,1,1,0,0,2],25),
  ([0,1,1,0,2,2,0],25),
  ([0,1,1,0,2,0,2],25),
  ([0,1,0,2,2,2,0],25),
  ([0,1,0,2,2,0,2],25),
  ([0,0,2,2,2,2,0],25),
  ([0,0,2,2,2,0,2],25),
  ([1,1,1,0,1,0,0],26),
  ([1,1,1,0,0,2,0],26),
  ([1,1,1,0,0,1,0],26),
  ([1,1,1,0,0,0,2],26),
  ([1,1,0,2,0,2,0],26),
  ([1,1,0,2,0,1,0],26),
  ([1,1,0,2,0,0,2],26),
  ([1,1,0,1,0,1,0],26),
  ([1,1,0,1,0,0,2],26),
  ([1,1,0,0,2,0,2],26),
  ([1,0,2,2,0,2,0],26),
  ([1,0,2,2,0,1,0],26),
  ([1,0,2,2,0,0,2],26),
  ([1,0,2,0,2,0,2],26),
  ([1,0,1,0,1,1,0],26),
  ([1,0,1,0,1,0,2],26),
  ([1,0,1,0,0,2,2],26),
  ([1,0,0,2,0,2,2],26),
  ([1,0,0,1,1,1,0],26),
  ([1,0,0,1,1,0,2],26),
  ([1,0,0,1,0,2,2],26),
  ([1,0,0,0,2,2,2],26),
  ([0,2,2,2,0,2,0],26),
  ([0,2,2,2,0,1,0],26),
  ([0,2,2,2,0,0,2],26),
  ([0,2,2,0,2,0,2],26),
  ([0,2,0,2,0,2,2],26),
  ([0,2,0,1,1,1,0],26),
  ([0,2,0,1,1,0,2],26),
  ([0,2,0,1,0,2,2],26),
  ([0,2,0,0,2,2,2],26),
  ([0,1,0,1,1,1,0],26),
  ([0,1,0,1,1,0,2],26),
  ([0,1,0,1,0,2,2],26),
  ([0,1,0,0,2,2,2],26),
  ([0,0,2,0,2,2,2],26),
  ([2,2,2,2,0,0,0],29),
  ([0,0,0,1,1,1,1],29),
  ([1,1,0,1,1,0,0],30),
  ([1,1,0,1,0,2,0],30),
  ([1,1,0,0,2,2,0],30),
  ([1,0,2,0,2,2,0],30),
  ([0,2,2,0,2,2,0],30),
  ([0,1,1,0,1,1,0],30),
  ([0,1,1,0,1,0,2],30),
  ([0,1,1,0,0,2,2],30),
  ([0,1,0,2,0,2,2],30),
  ([0,0,2,2,0,2,2],30),
  ([2,2,2,0,2,0,0],31),
  ([0,0,1,0,1,1,1],31),
  ([2,2,2,0,0,1,0],32),
  ([2,2,2,0,0,0,2],32),
  ([1,1,0,0,1,1,0],32),
  ([1,1,0,0,1,0,2],32),
  ([1,1,0,0,0,2,2],32),
  ([1,0,2,0,1,1,0],32),
  ([1,0,2,0,1,0,2],32),
  ([1,0,2,0,0,2,2],32),
  ([1,0,0,0,1,1,1],32),
  ([0,2,2,0,1,1,0],32),
  ([0,2,2,0,1,0,2],32),
  ([0,2,2,0,0,2,2],32),
  ([0,2,0,0,1,1,1],32),
  ([2,2,2,0,1,0,0],34),
  ([2,2,2,0,0,2,0],34),
  ([2,2,0,2,0,2,0],34),
  ([2,2,0,2,0,1,0],34),
  ([2,2,0,2,0,0,2],34),
  ([2,2,0,1,0,1,0],34),
  ([2,2,0,1,0,0,2],34),
  ([2,2,0,0,2,0,2],34),
  ([2,0,2,0,2,0,2],34),
  ([1,0,1,0,1,0,1],34),
  ([1,0,1,0,0,1,1],34),
  ([1,0,0,2,0,1,1],34),
  ([1,0,0,1,0,1,1],34),
  ([0,2,0,2,0,1,1],34),
  ([0,2,0,1,0,1,1],34),
  ([0,1,0,1,0,1,1],34),
  ([0,1,0,0,1,1,1],34),
  ([0,0,2,0,1,1,1],34),
  ([2,2,0,2,2,0,0],36),
  ([2,0,2,2,2,0,0],36),
  ([0,0,1,1,1,0,1],36),
  ([0,0,1,1,0,1,1],36),
  ([2,0,2,2,0,2,0],39),
  ([2,0,2,2,0,1,0],39),
  ([2,0,2,2,0,0,2],39),
  ([1,0,0,1,1,0,1],39),
  ([0,2,0,1,1,0,1],39),
  ([0,1,0,1,1,0,1],39),
  ([2,2,0,1,1,0,0],40),
  ([2,2,0,1,0,2,0],40),
  ([2,2,0,0,2,2,0],40),
  ([2,2,0,0,1,1,0],40),
  ([2,2,0,0,1,0,2],40),
  ([2,2,0,0,0,2,2],40),
  ([2,0,2,0,2,2,0],40),
  ([2,0,1,1,1,0,0],40),
  ([2,0,1,1,0,2,0],40),
  ([2,0,1,1,0,1,0],40),
  ([2,0,1,1,0,0,2],40),
  ([2,0,1,0,2,2,0],40),
  ([2,0,1,0,2,0,2],40),
  ([2,0,0,2,2,2,0],40),
  ([2,0,0,2,2,0,2],40),
  ([2,0,0,1,1,1,0],40),
  ([2,0,0,1,1,0,2],40),
  ([2,0,0,1,0,2,2],40),
  ([2,0,0,0,2,2,2],40),
  ([1,1,1,0,0,0,1],40),
  ([1,1,0,2,0,0,1],40),
  ([1,1,0,0,0,1,1],40),
  ([1,0,2,2,0,0,1],40),
  ([1,0,2,0,0,1,1],40),
  ([1,0,1,1,0,0,1],40),
  ([1,0,1,0,2,0,1],40),
  ([1,0,0,2,2,0,1],40),
  ([0,2,2,2,0,0,1],40),
  ([0,2,2,0,0,1,1],40),
  ([0,2,0,2,2,0,1],40),
  ([0,1,1,1,0,0,1],40),
  ([0,1,1,0,2,0,1],40),
  ([0,1,1,0,1,0,1],40),
  ([0,1,1,0,0,1,1],40),
  ([0,1,0,2,2,0,1],40),
  ([0,1,0,2,0,1,1],40),
  ([0,0,2,2,2,0,1],40),
  ([0,0,2,2,0,1,1],40),
  ([2,0,2,0,1,1,0],42),
  ([2,0,2,0,1,0,2],42),
  ([2,0,2,0,0,2,2],42),
  ([2,0,1,0,1,1,0],42),
  ([2,0,1,0,1,0,2],42),
  ([2,0,1,0,0,2,2],42),
  ([2,0,0,2,0,2,2],42),
  ([1,1,0,1,0,0,1],42),
  ([1,1,0,0,2,0,1],42),
  ([1,1,0,0,1,0,1],42),
  ([1,0,2,0,2,0,1],42),
  ([1,0,2,0,1,0,1],42),
  ([0,2,2,0,2,0,1],42),
  ([0,2,2,0,1,0,1],42),
  ([2,2,2,0,0,0,1],49),
  ([2,2,0,0,0,1,1],49),
  ([2,0,0,0,1,1,1],49),
  ([2,2,0,2,0,0,1],52),
  ([2,2,0,0,1,0,1],52),
  ([2,0,2,0,0,1,1],52),
  ([2,0,0,1,0,1,1],52),
  ([2,2,0,1,0,0,1],55),
  ([2,2,0,0,2,0,1],55),
  ([2,0,2,0,2,0,1],55),
  ([2,0,2,0,1,0,1],55),
  ([2,0,1,0,1,0,1],55),
  ([2,0,1,0,0,1,1],55),
  ([2,0,0,2,0,1,1],55),
  ([1,0,1,0,1,0,0],55),
  ([1,0,1,0,0,2,0],55),
  ([1,0,1,0,0,1,0],55),
  ([1,0,1,0,0,0,2],55),
  ([1,0,0,2,0,2,0],55),
  ([1,0,0,2,0,1,0],55),
  ([1,0,0,2,0,0,2],55),
  ([1,0,0,1,0,1,0],55),
  ([1,0,0,1,0,0,2],55),
  ([1,0,0,0,2,0,2],55),
  ([0,2,0,2,0,2,0],55),
  ([0,2,0,2,0,1,0],55),
  ([0,2,0,2,0,0,2],55),
  ([0,2,0,1,0,1,0],55),
  ([0,2,0,1,0,0,2],55),
  ([0,2,0,0,2,0,2],55),
  ([0,1,0,1,0,1,0],55),
  ([0,1,0,1,0,0,2],55),
  ([0,1,0,0,2,0,2],55),
  ([0,0,2,0,2,0,2],55),
  ([1,1,1,0,0,0,0],58),
  ([1,1,0,2,0,0,0],58),
  ([1,0,2,2,0,0,0],58),
  ([0,2,2,2,0,0,0],58),
  ([0,0,0,1,1,1,0],58),
  ([0,0,0,1,1,0,2],58),
  ([0,0,0,1,0,2,2],58),
  ([0,0,0,0,2,2,2],58),
  ([2,0,2,2,0,0,1],60),
  ([2,0,0,1,1,0,1],60),
  ([1,0,1,1,0,0,0],60),
  ([1,0,1,0,2,0,0],60),
  ([1,0,0,2,2,0,0],60),
  ([0,2,0,2,2,0,0],60),
  ([0,1,1,1,0,0,0],60),
  ([0,1,1,0,2,0,0],60),
  ([0,1,0,2,2,0,0],60),
  ([0,0,2,2,2,0,0],60),
  ([0,0,1,1,1,0,0],60),
  ([0,0,1,1,0,2,0],60),
  ([0,0,1,1,0,1,0],60),
  ([0,0,1,1,0,0,2],60),
  ([0,0,1,0,2,2,0],60),
  ([0,0,1,0,2,0,2],60),
  ([0,0,0,2,2,2,0],60),
  ([0,0,0,2,2,0,2],60),
  ([1,1,0,1,0,0,0],62),
  ([1,1,0,0,2,0,0],62),
  ([1,0,2,0,2,0,0],62),
  ([0,2,2,0,2,0,0],62),
  ([0,0,1,0,1,1,0],62),
  ([0,0,1,0,1,0,2],62),
  ([0,0,1,0,0,2,2],62),
  ([0,0,0,2,0,2,2],62),
  ([2,0,1,1,0,0,1],64),
  ([2,0,1,0,2,0,1],64),
  ([2,0,0,2,2,0,1],64),
  ([1,1,0,0,0,1,0],64),
  ([1,1,0,0,0,0,2],64),
  ([1,0,2,0,0,1,0],64),
  ([1,0,2,0,0,0,2],64),
  ([1,0,0,0,1,1,0],64),
  ([1,0,0,0,1,0,2],64),
  ([1,0,0,0,0,2,2],64),
  ([0,2,2,0,0,1,0],64),
  ([0,2,2,0,0,0,2],64),
  ([0,2,0,0,1,1,0],64),
  ([0,2,0,0,1,0,2],64),
  ([0,2,0,0,0,2,2],64),
  ([1,0,0,1,1,0,0],65),
  ([1,0,0,1,0,2,0],65),
  ([1,0,0,0,2,2,0],65),
  ([0,2,0,1,1,0,0],65),
  ([0,2,0,1,0,2,0],65),
  ([0,2,0,0,2,2,0],65),
  ([0,1,1,0,1,0,0],65),
  ([0,1,1,0,0,2,0],65),
  ([0,1,1,0,0,1,0],65),
  ([0,1,1,0,0,0,2],65),
  ([0,1,0,2,0,2,0],65),
  ([0,1,0,2,0,1,0],65),
  ([0,1,0,2,0,0,2],65),
  ([0,1,0,1,1,0,0],65),
  ([0,1,0,1,0,2,0],65),
  ([0,1,0,0,2,2,0],65),
  ([0,0,2,2,0,2,0],65),
  ([0,0,2,2,0,1,0],65),
  ([0,0,2,2,0,0,2],65),
  ([0,0,2,0,2,2,0],65),
  ([1,1,0,0,1,0,0],68),
  ([1,1,0,0,0,2,0],68),
  ([1,0,2,0,1,0,0],68),
  ([1,0,2,0,0,2,0],68),
  ([0,2,2,0,1,0,0],68),
  ([0,2,2,0,0,2,0],68),
  ([0,1,0,0,1,1,0],68),
  ([0,1,0,0,1,0,2],68),
  ([0,1,0,0,0,2,2],68),
  ([0,0,2,0,1,1,0],68),
  ([0,0,2,0,1,0,2],68),
  ([0,0,2,0,0,2,2],68),
  ([2,2,2,0,0,0,0],70),
  ([0,0,0,0,1,1,1],70),
  ([2,2,0,2,0,0,0],75),
  ([0,0,0,1,0,1,1],75),
  ([2,2,0,0,0,1,0],78),
  ([2,2,0,0,0,0,2],78),
  ([1,0,0,0,0,1,1],78),
  ([0,2,0,0,0,1,1],78),
  ([2,2,0,1,0,0,0],81),
  ([2,2,0,0,2,0,0],81),
  ([2,0,2,0,2,0,0],81),
  ([0,0,1,0,1,0,1],81),
  ([0,0,1,0,0,1,1],81),
  ([0,0,0,2,0,1,1],81),
  ([2,0,2,0,0,1,0],83),
  ([2,0,2,0,0,0,2],83),
  ([1,0,0,0,1,0,1],83),
  ([0,2,0,0,1,0,1],83),
  ([2,2,0,0,1,0,0],84),
  ([2,2,0,0,0,2,0],84),
  ([2,0,0,1,0,1,0],84),
  ([2,0,0,1,0,0,2],84),
  ([2,0,0,0,2,0,2],84),
  ([1,0,1,0,0,0,1],84),
  ([1,0,0,2,0,0,1],84),
  ([0,2,0,2,0,0,1],84),
  ([0,1,0,0,0,1,1],84),
  ([0,0,2,0,0,1,1],84),
  ([2,0,2,2,0,0,0],87),
  ([0,0,0,1,1,0,1],87),
  ([2,0,2,0,1,0,0],89),
  ([2,0,2,0,0,2,0],89),
  ([2,0,1,0,1,0,0],89),
  ([2,0,1,0,0,2,0],89),
  ([2,0,1,0,0,1,0],89),
  ([2,0,1,0,0,0,2],89),
  ([2,0,0,2,0,2,0],89),
  ([2,0,0,2,0,1,0],89),
  ([2,0,0,2,0,0,2],89),
  ([1,0,0,1,0,0,1],89),
  ([1,0,0,0,2,0,1],89),
  ([0,2,0,1,0,0,1],89),
  ([0,2,0,0,2,0,1],89),
  ([0,1,0,1,0,0,1],89),
  ([0,1,0,0,2,0,1],89),
  ([0,1,0,0,1,0,1],89),
  ([0,0,2,0,2,0,1],89),
  ([0,0,2,0,1,0,1],89),
  ([2,0,1,1,0,0,0],96),
  ([2,0,1,0,2,0,0],96),
  ([2,0,0,2,2,0,0],96),
  ([0,0,1,1,0,0,1],96),
  ([0,0,1,0,2,0,1],96),
  ([0,0,0,2,2,0,1],96),
  ([2,0,0,0,1,1,0],98),
  ([2,0,0,0,1,0,2],98),
  ([2,0,0,0,0,2,2],98),
  ([1,1,0,0,0,0,1],98),
  ([1,0,2,0,0,0,1],98),
  ([0,2,2,0,0,0,1],98),
  ([2,0,0,1,1,0,0],100),
  ([2,0,0,1,0,2,0],100),
  ([2,0,0,0,2,2,0],100),
  ([0,1,1,0,0,0,1],100),
  ([0,1,0,2,0,0,1],100),
  ([0,0,2,2,0,0,1],100),
  ([2,2,0,0,0,0,1],119),
  ([2,0,0,0,0,1,1],119),
  ([1,0,1,0,0,0,0],121),
  ([1,0,0,2,0,0,0],121),
  ([0,2,0,2,0,0,0],121),
  ([0,0,0,1,0,1,0],121),
  ([0,0,0,1,0,0,2],121),
  ([0,0,0,0,2,0,2],121),
  ([1,0,0,0,0,1,0],124),
  ([1,0,0,0,0,0,2],124),
  ([0,2,0,0,0,1,0],124),
  ([0,2,0,0,0,0,2],124),
  ([2,0,2,0,0,0,1],127),
  ([2,0,0,0,1,0,1],127),
  ([1,0,0,1,0,0,0],131),
  ([1,0,0,0,2,0,0],131),
  ([0,2,0,1,0,0,0],131),
  ([0,2,0,0,2,0,0],131),
  ([0,1,0,1,0,0,0],131),
  ([0,1,0,0,2,0,0],131),
  ([0,0,2,0,2,0,0],131),
  ([0,0,1,0,1,0,0],131),
  ([0,0,1,0,0,2,0],131),
  ([0,0,1,0,0,1,0],131),
  ([0,0,1,0,0,0,2],131),
  ([0,0,0,2,0,2,0],131),
  ([0,0,0,2,0,1,0],131),
  ([0,0,0,2,0,0,2],131),
  ([1,0,0,0,1,0,0],134),
  ([1,0,0,0,0,2,0],134),
  ([0,2,0,0,1,0,0],134),
  ([0,2,0,0,0,2,0],134),
  ([0,1,0,0,0,1,0],134),
  ([0,1,0,0,0,0,2],134),
  ([0,0,2,0,0,1,0],134),
  ([0,0,2,0,0,0,2],134),
  ([2,0,1,0,0,0,1],136),
  ([2,0,0,2,0,0,1],136),
  ([2,0,0,1,0,0,1],136),
  ([2,0,0,0,2,0,1],136),
  ([1,1,0,0,0,0,0],140),
  ([1,0,2,0,0,0,0],140),
  ([0,2,2,0,0,0,0],140),
  ([0,0,0,0,1,1,0],140),
  ([0,0,0,0,1,0,2],140),
  ([0,0,0,0,0,2,2],140),
  ([0,1,0,0,1,0,0],144),
  ([0,1,0,0,0,2,0],144),
  ([0,0,2,0,1,0,0],144),
  ([0,0,2,0,0,2,0],144),
  ([0,0,1,1,0,0,0],144),
  ([0,0,1,0,2,0,0],144),
  ([0,0,0,2,2,0,0],144),
  ([0,1,1,0,0,0,0],145),
  ([0,1,0,2,0,0,0],145),
  ([0,0,2,2,0,0,0],145),
  ([0,0,0,1,1,0,0],145),
  ([0,0,0,1,0,2,0],145),
  ([0,0,0,0,2,2,0],145),
  ([2,2,0,0,0,0,0],169),
  ([0,0,0,0,0,1,1],169),
  ([2,0,2,0,0,0,0],181),
  ([0,0,0,0,1,0,1],181),
  ([2,0,0,0,0,1,0],189),
  ([2,0,0,0,0,0,2],189),
  ([1,0,0,0,0,0,1],189),
  ([0,2,0,0,0,0,1],189),
  ([2,0,1,0,0,0,0],196),
  ([2,0,0,2,0,0,0],196),
  ([0,0,0,1,0,0,1],196),
  ([0,0,0,0,2,0,1],196),
  ([2,0,0,1,0,0,0],200),
  ([2,0,0,0,2,0,0],200),
  ([0,0,1,0,0,0,1],200),
  ([0,0,0,2,0,0,1],200),
  ([2,0,0,0,1,0,0],205),
  ([2,0,0,0,0,2,0],205),
  ([0,1,0,0,0,0,1],205),
  ([0,0,2,0,0,0,1],205),
  ([1,0,0,0,0,0,0],268),
  ([0,2,0,0,0,0,0],268),
  ([0,0,0,0,0,1,0],268),
  ([0,0,0,0,0,0,2],268),
  ([2,0,0,0,0,0,1],288),
  ([0,0,1,0,0,0,0],288),
  ([0,0,0,2,0,0,0],288),
  ([0,0,0,1,0,0,0],288),
  ([0,0,0,0,2,0,0],288),
  ([0,1,0,0,0,0,0],292),
  ([0,0,2,0,0,0,0],292),
  ([0,0,0,0,1,0,0],292),
  ([0,0,0,0,0,2,0],292),
  ([2,0,0,0,0,0,0],408),
  ([0,0,0,0,0,0,1],408),
  ([0,0,0,0,0,0,0],577)]
