module RayTracer.Constants (size, center, seed) where
{--
size   ary_size   time          rays
19  MB 100^3      real: 14 secs 1M
302 MB 250^3      real: 42 secs 1M
2.42GB 500^3      real: 2m 3sec 1M
6.65GB 700^3      user: 7m  13s 1M
19.4GB 1000^3     real: 16m 30s 1M
--}

center:: Double
center = fromIntegral size /  2

seed, size :: Int
seed = 8675309
size = 1000

type AryDim = Double
type Mins = String

quadReg :: AryDim -> Mins
quadReg x =
  let a = 64.759143 in
  let b = -0.561470164 in
  let c = 0.001539990375 in
  let quad = a + b * x + c * x**2 in
  show (quad / 60) ++ " Minutes"