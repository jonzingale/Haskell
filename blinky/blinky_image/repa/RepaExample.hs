{-# LANGUAGE TypeOperators #-}

module RepaExample where
import Data.Array.Repa as R

-- Will need to dockerize a linux box with CUDA or LLVM to
-- run actual GPU backend

arrayA = fromListUnboxed (ix1 100) [1..100 ::Int]
arrayB = R.map (+ 10) arrayA

val = arrayB ! ix1 0
