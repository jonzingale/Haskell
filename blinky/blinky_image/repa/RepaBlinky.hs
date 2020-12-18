{-# LANGUAGE TypeOperators, QuasiQuotes #-}

module RepaBlinky where
import Data.Array.Repa as R
import Data.Array.Repa.Eval as R
import Data.Array.Repa (Z(..), (:.)(..))
import Data.Array.Repa.Stencil -- Stencil
import Data.Array.Repa.Stencil.Dim2 -- stencil2
-- import Data.Array.Repa.Algorithms.Convolve as C

-- cabal install repa-3.4.1.4

-- What is program? show2D?
-- https://www.slideshare.net/kizzx2/repagolpdf
gameOfLife :: p -> IO ()
gameOfLife world = do
--   show2D 6 world
  input <- getLine
  case input of
    "q" -> return ()
    _ -> return ()
    -- _ -> program $ tick world

tick :: Array U DIM2 Int -> IO (Array U DIM2 Int)
tick world = R.computeP $ R.zipWith transit world neighbors
  where neighbors = mapStencil2 (BoundConst 0) sten world

transit :: Int -> Int -> Int
transit 1 2 = 1
transit 1 3 = 1
transit 1 _ = 0
transit 0 3 = 1
transit 0 _ = 0

sten :: Stencil DIM2 Int
sten = [stencil2| 1 1 1
                  1 0 1
                  1 1 1 |]

toad :: Array U DIM2 Int
toad = R.fromListUnboxed (Z :. 6 :. 6 :: DIM2)
  [0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0,
   0, 0, 1, 1, 1, 0,
   0, 1, 1, 1, 0, 0,
   0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0 :: Int
  ]
