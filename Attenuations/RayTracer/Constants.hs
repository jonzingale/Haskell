module RayTracer.Constants where
{--
center @ GaussianBeam
size   @ ParallelTracer
size   @ PhotographicPlate
range(0, size**3) @ visualizer.py
--}

size = 100::Int

center :: Double
center = (fromIntegral size) /  2

-- Number of rays desired by coupon collection.
raySize = 1*10**6 -- default
-- raySize | size < 500 = 1*10**6
--         | otherwise =
--           let s = (fromIntegral size)::Double in
--           (s*s) * log (s*s)