
{-# OPTIONS_GHC -Wno-missing-methods #-} -- Floating and Fractional

module Bit where
import Complex

data Bit = One | Zero | Bad deriving (Show, Eq)

class ZTwo m where
  flip :: m -> m
  val :: m -> Integer

instance ZTwo Bit where
  flip Zero = One
  flip One = Zero
  val Zero = 0
  val One = 1

instance Num Bit where
  (+) a Zero = a
  (+) Zero a = a
  (+) One One = Zero
  (*) a Zero = Zero
  (*) Zero a = Zero
  (*) One One = One
  negate Zero = One
  negate One = Zero
  signum Zero = 0
  signum One = 1
  fromInteger 0 = Zero
  fromInteger 1 = One
  abs = id

instance Floating Bit where
  sqrt a = a

instance Fractional Bit where
  (/) _ Zero = Bad
  (/) _ One = One

instance Comp Bit where
  conj = id
