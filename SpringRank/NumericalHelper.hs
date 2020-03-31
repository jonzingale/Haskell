{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module NumericalHelper where

import Numeric.LinearAlgebra.Class (AdditiveGroup, (^+^), (^-^))
import Data.Sparse.SpMatrix (SpMatrix)
import Data.Sparse.SpVector (SpVector)

instance (Num a, AdditiveGroup a) => Num (SpMatrix a) where
  (+) = (^+^)
  (-) = (^-^)

instance (Num a, AdditiveGroup a) => Num (SpVector a) where
  (+) = (^+^)
  (-) = (^-^)