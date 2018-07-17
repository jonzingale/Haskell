{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.GaussianTests where
import Tests.ExplicitGenerators
import RayTracer.GaussianBeam
import Test.Framework

prop_DiagonalAngles :: Gen Bool
prop_DiagonalAngles = do
  d <- interval
  let ray' = ray d (sqrt 2/2, sqrt 2/2) -- only works for d==1
  let (θ, φ) = snd ray'
  let c = 0.6154797086703874
  return $ eBall 13 θ c && eBall 13 φ c

