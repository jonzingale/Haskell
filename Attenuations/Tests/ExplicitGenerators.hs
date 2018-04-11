module Tests.ExplicitGenerators where
import Test.Framework

-- Constants
tau = 2 * pi

-- Explicit Generators
interval = choose (0, 1::Double)
zeroToPi = choose (0, pi::Double)
zeroToTau =choose (0, tau::Double)

{--
  δ β
ε_\|/_α
--}
epsilonRegion x = choose (3*pi/4 + x*pi/4, pi) -- super-εδ-Condition
deltaRegion x = choose (pi/2, pi/2 + x*pi/4) -- sub-εδ-Condition
betaRegion  x = choose ((1+x) * pi/4, pi/2) -- super-αβ-Condition
alphaRegion x = choose (0, (1+x) * pi/4) -- sub-αβ-Condition

-- Tolerances
type DoubleCoords =  ((Double, Double), Double)
type IntegerCoords = ((Integer, Integer), Integer)

--better would be an epsilon ball
tol :: Double -> Integer
tol d = round $ d * 10^12

mtol :: DoubleCoords -> IntegerCoords
mtol ((x,y), t) = ((tol x, tol y), tol t)