{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.GaussianTests where
import Tests.ExplicitGenerators
import RayTracer.GaussianBeam
import Test.Framework

-- Given a unit cone, diagonals are what I expect.
prop_DiagonalAngles :: Gen Bool
prop_DiagonalAngles = do
  s <- oneof [return 1, return (-1)]
  r <- oneof [return 1, return (-1)]
  d <- distance
  let ray' = ray d (s*d*sqrt 2/2, r*d*sqrt 2/2)
  let dAngle = 0.6154797086703874
  let (θ, φ) = snd ray'
  return $ eBall 13 θ (s*dAngle) && eBall 13 φ (r*dAngle)

