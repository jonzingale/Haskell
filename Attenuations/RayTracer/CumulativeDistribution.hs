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