
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}

module Tests.SpecificRayTests where
import Tests.ExplicitGenerators
import RayTracer.Crossings
import RayTracer.Transport
import Test.Framework


prop_sθ_sφ_x_no_z = do
  s <- choose (1, 20::Int)
  let rs = (fromIntegral s)::Double
  let ijkSeg = transport (1/2, 0) (atan 2, atan(2/sqrt 5))
  let eval = sum [ seg | (_, seg) <- takeWhile (stopCond s) ijkSeg]
  return $ (eBall 13) eval (rs * sqrt (1 + (2/sqrt 5)**2))
  where
    stopCond s ((x,y,z), _) = x< s && y< s && z< s

prop_sθ_sφ_xz = do
  s <- choose (1, 20::Int)
  let rs = (fromIntegral s)::Double
  let ijkSeg = transport (1/3, 1/4) (atan (3/2), atan(4*sqrt 13 / 9))
  let eval = sum [ seg | (_, seg) <- takeWhile (stopCond s) ijkSeg]
  return $ (eBall 13) eval (rs * 17/12)
  where
    stopCond s ((x,y,z), _) = x>=0 && y<s && z<s

prop_lθ_sφ_xz = do -- flaky
  t <- squareEvenInt -- includes 1000
  let s = 40::Int
  let rs = (fromIntegral s)::Double
  let ijkSeg = transport (rs*3/4, rs*1/4) (pi-atan(4/3), atan(5/3))
  let eval = sum [ seg | (_, seg) <- takeWhile (stopCond s) ijkSeg]
  return $ (eBall 13) eval (rs * sqrt 34 / 4)
  where
    stopCond s ((x,y,z), _) = x>=0 && y<s && z<s

prop_lθ_lφ_xz = do -- flaky though
  t <- multiplesof20Int -- includes 1000 though it breaks
  let s = 40::Int
  let rs = (fromIntegral s)::Double
  let ijkSeg = transport (rs*3/4, rs*4/5) (pi-atan(4/3), pi- atan(25/16))
  let eval = sum [ seg | (_, seg) <- takeWhile (stopCond s) ijkSeg]
  return $ (eBall 13) eval (rs * sqrt 881 / 20)
  where
    stopCond s ((x,y,z), _) = x>=0 && x<s && y<s && z>=0 && z<s

-- prop_lθ_lφ_xz' = do
--   s <- choose (7, 20::Int)
--   let ijkSeg = transport (3/4, 2/3) (pi-atan(4/3), atan(15/8))
--   let eval = sum [ seg | (_, seg) <- takeWhile (stopCond s) ijkSeg]
--   return $ (eBall 13) eval (1 * 17/12)
--   where
--     stopCond s ((x,y,z), _) = x>=0 && y<1 && z>=0

prop_sθ_lφ_xz = do
  s <- choose (7, 20::Int)
  let rs = (fromIntegral s)::Double
  let ijkSeg = transport (rs*1/4, rs*2/3) (atan (4/3), pi - atan(15/8))
  let eval = sum [ seg | (_, seg) <- takeWhile (stopCond s) ijkSeg]
  return $ (eBall 10) eval (rs * 17/12)
  where
    stopCond s ((x,y,z), _) = x>=0 && y<s && z >= 0

-- prop_arbitrary_coords = do
--   s <- choose (7, 20::Int)
--   let ijkSeg = transport (1/5, 5/6) (pi/4, pi - atan (96*sqrt 2 / 70))
--   let eval = sum [ seg | (_, seg) <- takeWhile (stopCond s) ijkSeg]
--   return $ (eBall 10) eval (1 * sqrt 5833 / 60)
--   where
--     stopCond s ((x,y,z), _) = x<1 && y<1 && z >= 0