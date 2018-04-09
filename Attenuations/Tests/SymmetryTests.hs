
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.SymmetryTests where
import RayTracer.RayLength
import Test.Framework

{--
Truly generalized algebraic testing.

import Text.Show.Functions

prop_ComposeAssoc f g h x =
  ((f . g) . h) x == (f . (g . h)) x
  where types = [f, g, h] :: [Int->Int]
--}

--tolerance 12 decimal places
--better would be an epsilon ball
tol :: Double -> Integer
tol d = round $ d * 10^12

mtol ((x,y), t) = ((tol x, tol y), tol t)

-- Rotation Tests
prop_rot90 = do
  θ <- choose (0, pi)
  return $ (tol.cos) (pi/2 - θ) == (tol.sin) θ

prop_rot4Id x y = do
  t <- choose (0, 2*pi)
  return $ (mtol.divPi.rotFour) ((x, y),t) == mtol ((x, y),t)
  where
    divPi ((x, y),theta) = ((x, y), theta - (2*pi))
    rotFour = foldr (.) id [rot90 | x<-[1..4]]

prop_rotInv' x y t = let coords = ((x, y),t) in
  (mtol.rot90.rot270) coords == mtol coords

prop_rotInv x y t = let coords = ((x, y),t) in
  (mtol.rot270.rot90) coords == mtol coords