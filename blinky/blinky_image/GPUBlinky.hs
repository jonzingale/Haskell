{-# LANGUAGE TypeOperators #-}

module GPUBlinky where
import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native -- Multicore CPUs

-- Will need to dockerize a linux box with CUDA or LLVM to
-- run actual GPU backend

type Matx = Array DIM2 Double

matProduct :: Acc Matx -> Acc Matx -> Acc Matx
matProduct a b = let
  Z :. mx :. _ = unlift (shape a) :: Z :. Exp Int :. Exp Int
  Z :. _ :. my = unlift (shape b) :: Z :. Exp Int :. Exp Int

  aRep = A.replicate (lift $ Z :. All :. my :. All) a
  bRep = A.replicate (lift $ Z :. mx :. All :. All) (A.transpose b)

  in A.fold (+) 0
    $ A.zipWith (*) aRep bRep

arrayA = lift $ fromList (Z :. 3) [1..] :: Acc (Array DIM1 Int)
arrayB = lift $ fromList (Z :. 3) [4..] :: Acc (Array DIM1 Int)

-- Library not loaded: /usr/local/opt/libffi/lib/libffi.6.dylib
array =  run $ generate (index2 3 3) (
  \ix -> let Z :. x :. y = unlift ix in arrayA A.!! x + arrayB A.!!y
  )