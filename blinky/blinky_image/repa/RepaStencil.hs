{-# LANGUAGE TypeOperators, QuasiQuotes #-}

module RepaStencil where
import Data.Array.Repa as R
import Data.Array.Repa.Stencil as S
import Data.Array.Repa.Stencil.Dim2 as SD
import Data.Array.Repa.Algorithms.Convolve as C

-- http://www.cse.chalmers.se/edu/year/2015/course/DAT280_Parallel_Functional_Programming/Papers/RepaTutorial13.pdf

type Matx = Array DIM2 Double

arrayA = fromListUnboxed (ix1 100) [1..100 ::Int]
arrayB = R.map (+ 10) arrayA

val = arrayB ! ix1 0

ker = makeStencil 3 (\x -> Just ([0,1,0]!!x))

lightStencil :: Stencil DIM2 Double
lightStencil =
  [stencil2|  0 1 0
              1 0 1
              0 1 0 |]

gaussStencil :: Stencil DIM2 Double
gaussStencil =
  [stencil2| 2  4  5  4 2
             4  9 12  9 4
             5 12 15 12 5
             4  9 12  9 4
             2  4  5  4 2 |]