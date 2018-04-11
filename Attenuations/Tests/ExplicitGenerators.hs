module Tests.ExplicitGenerators where
import Test.Framework


interval = choose (0, 1::Double)

epsilonRegion x = choose (3*pi/4 + x*pi/4, pi) -- super-εδ-Condition

deltaRegion x = choose (pi/2, pi/2 + x*pi/4) -- sub-εδ-Condition
