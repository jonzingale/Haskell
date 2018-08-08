module RayTracer.Constants (size, center, raySize) where
{--
center @ GaussianBeam
size   @ ParallelTracer
size   @ PhotographicPlate
range(0, size**3) @ visualizer.py

size   ary_size   time          rays
19  MB 100^3      real: 14 secs 1M
302 MB 250^3      real: 42 secs 1M
2.42GB 500^3      real: 2m31sec 1M
6.65GB 700^3      user: 7m13sec 1M
19.4GB 1000^3     real: 48 mins 1M *

19  MB 100^3      user: 1.6mins 3M
302 MB 250^3      real: 9 mins  3M
2.42GB 500^3      user: 30mins  3M
19.4GB 1000^3     out-of-range  3M
--}

center, raySize :: Double
center = fromIntegral size /  2
raySize = 1*10**6
size = 1000::Int
