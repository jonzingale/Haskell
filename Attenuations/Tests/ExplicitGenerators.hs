module Tests.ExplicitGenerators where
import Test.Framework

-- Constants
tau = 2 * pi

-- Explicit Generators
interval = choose (0, 1::Double)

epsilonRegion x = choose (3*pi/4 + x*pi/4, pi) -- super-εδ-Condition

deltaRegion x = choose (pi/2, pi/2 + x*pi/4) -- sub-εδ-Condition


-- Tolerances
type DoubleCoords =  ((Double, Double), Double)
type IntegerCoords = ((Integer, Integer), Integer)

--better would be an epsilon ball
tol :: Double -> Integer
tol d = round $ d * 10^12

mtol :: DoubleCoords -> IntegerCoords
mtol ((x,y), t) = ((tol x, tol y), tol t)