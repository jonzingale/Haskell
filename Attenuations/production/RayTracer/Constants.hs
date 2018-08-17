module RayTracer.Constants where
{--
size   ary_size   time          rays
19  MB 100^3      real: 14 secs 1M
302 MB 250^3      real: 42 secs 1M
2.42GB 500^3      real: 2m 3sec 1M
6.65GB 700^3      user: 7m  13s 1M
19.4GB 1000^3     real: 12m 30s 1M
--}

center:: Double
center = 500

size :: Int
size = 1000
