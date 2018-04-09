{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.SymmetryTests where
import RayTracer.RayLength
import Test.Framework

replayArg = "Just (TFGenR 15067B55359906C0776B9C0A73ACEE7D9C124B4AE3DAC3AFCB451E04B1EF7BD1 0 31 5 0,28)"
pt = ((0.0, 0.0), -63.8735667731305)
prop_rot4IdReplay =
  withQCArgs (\a -> a { replay = read replayArg })
  prop_rot4Id

-- Rotation Tests
prop_rot90 = do
  x <- choose (0, pi)
  return $ (tol.cos) (pi/2 - x) == (tol.sin) x