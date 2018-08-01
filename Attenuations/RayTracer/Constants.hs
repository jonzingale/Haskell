module RayTracer.Constants where
{--
center @ GaussianBeam
size   @ ParallelTracer
size   @ PhotographicPlate
range(0, size**3) @ visualizer.py
--}

size = 500::Int

center :: Double
center = (fromIntegral size) /  2