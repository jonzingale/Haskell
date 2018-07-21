-- http://hackage.haskell.org/package/statistics-0.14.0.2/docs/Statistics-Distribution.html
-- http://hackage.haskell.org/package/statistics-0.14.0.2/docs/Statistics-Distribution-Normal.html#t:NormalDistribution
module RayTracer.CumulativeDistribution where
import Statistics.Distribution.Normal
import Statistics.Distribution

{--
Here I would like to calculate the probability
that a ray will land outside of a given lattice.
--}
type Center = Double
type Deviation = Double

getProb :: Center -> Deviation -> Double -> Double 
getProb μ σ x = cumulative (normalDistr μ σ) x 

getStdProb :: Double -> Double
getStdProb x = cumulative standard x

{--
P { 2 < X < 5 }, μ = 3 σ = 9
P { (2-3)/3 < (x-3)/3 < (5-3)/3 }
P { -1/3 < Z < 2/3 }
φ (2/3) - φ (-1/3)
φ (2/3) - [ 1 - φ (1/3) ] ~ .3779
--}
testProb :: Double
testProb =
  let l = getStdProb (2/3) in
  let r = getStdProb (-1/3) in
  l - r