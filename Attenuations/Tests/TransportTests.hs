
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.SymmetryTests where
import Tests.ExplicitGenerators
import RayTracer.FileToVector
import RayTracer.Transport
import Test.Framework

allOnes = fileToAry "./Tests/dataTestAllOnes"

-- prop_rot4Id :: Point -> Gen Bool
-- prop_rot4Id cs = do
--   t <- zeroToTau
--   return $ (mtol.diffPi.rotFour) (cs, t) == mtol (cs, t)
--   where
--     diffPi ((x, y), theta) = ((x, y), theta - tau)
--     rotFour = foldr (.) id [rot90 | x<-[1..4]]


-- prop_reflectInv :: (Point, Angle) -> Bool
-- prop_reflectInv cs = (mtol.reflectY.reflectY) cs == mtol cs

-- prop_RhoIsReflectedRho' :: Gen Bool
-- prop_RhoIsReflectedRho' = do
--   y <- interval
--   t <- rhoRegion y
--   let (csR, tR) = reflectY ((0,y), t)
--   return $ (eBall 13) (rho (0,y) t) (rho' csR tR)

