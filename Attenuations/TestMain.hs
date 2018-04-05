
-- http://hackage.haskell.org/package/HTF-0.13.2.4/docs/Test-Framework-Tutorial.html
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AttenuationTest where
import RayTracer.RayLength
import RayTracer.Lattice
import RayTracer.Rhythm
import Test.Framework

--- Attenuation Tests.

-- rayLength
test_ExpectedValues :: IO ()
test_ExpectedValues = do
    let smxsmθ = ((3/4, 0), pi/6)
    let smxlgθ = ((1/3, 0), pi/3)
    let smxOneθ = ((1/4, 0), pi/2)
    let smalls = [smxlgθ, smxsmθ, smxOneθ]
    let vals = map (uncurry rayLength) smalls
    assertEqual vals vals

test_diagonalSymmetry :: IO () -- rewrite as prop test
test_diagonalSymmetry = do
  let over45  = rayLength (0,0) (pi*3/8)
  let under45 = rayLength (0,0) (pi*5/8)
  assertEqual over45 under45

test_offDiagonalSymmetry :: IO () -- rewrite as prop test
test_offDiagonalSymmetry = do
  let over45  = rayLength (2/3,0) (pi*3/8)
  let under45 = rayLength (0,2/3) (pi*3/8)
  assertEqual over45 under45

-- toAngleDeg
prop_radiansToDeg :: Slope -> Bool
prop_radiansToDeg (n,d) = toAngleDeg (n,d) == toAngleRad (n,d) * 180 / pi

--- RayTracer Tests.
test_rabbits :: IO ()
test_rabbits = do
  let rhythm = take 15 $ rabbits (5,3)
  let rhythmData = ".rLrrLr.rLrrLr."
  assertEqual rhythm rhythmData



main = htfMain htf_thisModulesTests