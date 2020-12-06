module Constants where

{--
It appears that blinks are linear in cost,
while pcount and bsize are more expensive.
many of the runs never exhaust their free
particles: ~24k. pcount should likely stay
smaller until blinks catch up.
--}

-- 85m52.939s, 27k
-- bsize = 700 :: Int -- board size
-- pcount = 50000 :: Int -- number of particles
-- blinks = 10000 :: Int -- number of iterations
-- hsize = div bsize 2 -- half a board size

-- 5m48.733s
bsize = 400 :: Int -- board size
pcount = 20000 :: Int -- number of particles
blinks = 3000 :: Int -- number of iterations
hsize = div bsize 2 -- half a board size

-- 0m23.250s, profiler
-- bsize = 300 :: Int -- board size
-- pcount = 5000 :: Int -- number of particles
-- blinks = 3000 :: Int -- number of iterations
-- hsize = div bsize 2 -- half a board size

-- 10m46.220s
-- bsize = 400 :: Int -- board size
-- pcount = 20000 :: Int -- number of particles
-- blinks = 6000 :: Int -- number of iterations
-- hsize = div bsize 2 -- half a board size

-- 51m31.191s, 11k
-- bsize = 700 :: Int -- board size
-- pcount = 30000 :: Int -- number of particles
-- blinks = 20000 :: Int -- number of iterations
-- hsize = div bsize 2 -- half a board size

-- 13m33.503s
-- bsize = 700 :: Int -- board size
-- pcount = 30000 :: Int -- number of particles
-- blinks = 10000 :: Int -- number of iterations
-- hsize = div bsize 2 -- half a board size

-- 180m48.976s, 24k
-- bsize = 800 :: Int -- board size
-- pcount = 50000 :: Int -- number of particles
-- blinks = 20000 :: Int -- number of iterations
-- hsize = div bsize 2 -- half a board size

-- 639m41.825s, 90k
-- bsize = 900 :: Int -- board size
-- pcount = 80000 :: Int -- number of particles
-- blinks = 30000 :: Int -- number of iterations
-- hsize = div bsize 2 -- half a board size
