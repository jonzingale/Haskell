{-# LANGUAGE TypeOperators #-}

module RepaBlinky where
import Data.Array.Repa as R

-- Will need to dockerize a linux box with CUDA or LLVM to
-- run actual GPU backend

type Matx = Array DIM2 Double

-- matProduct :: Acc Matx -> Acc Matx -> Acc Matx
-- matProduct a b = let
--   Z :. mx :. _ = unlift (shape a) :: Z :. Exp Int :. Exp Int
--   Z :. _ :. my = unlift (shape b) :: Z :. Exp Int :. Exp Int

--   aRep = R.replicate (lift $ Z :. All :. my :. All) a
--   bRep = R.replicate (lift $ Z :. mx :. All :. All) (R.transpose b)

--   in R.foldP (+) 0
--     $ R.zipWith (*) aRep bRep

arrayA = fromListUnboxed (ix1 100) [1..100 ::Int]
arrayB = R.map (+ 10) arrayA

val = arrayB ! ix1 0
