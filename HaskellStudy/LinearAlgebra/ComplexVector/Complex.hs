-- Btw, there exists Data.Complex with its (a :+ b) notation
{-# OPTIONS_GHC -Wno-missing-methods #-} -- because of Floating, signum in Num.

module Complex where
import Text.Printf

c1, c2 :: Complex
c1 = C 3 1
c2 = C 4 (-2) 

data Complex = C {real::Double, imag::Double} deriving (Eq)

instance Show Complex where
  show (C a b) | b < 0 = printf "%f%fi" a b
               | b == 0 = printf "%f+%fi" a (0.0::Double)
               | otherwise = printf "%f+%fi" a b

class Comp c where
  conj :: c -> c
  incl :: Double -> c

instance Comp Double where
  conj = id
  incl = id

instance Comp Complex where
  conj (C a b) = C a (-b)
  incl x = C x 0

instance Num Complex where
  fromInteger x = C (fromInteger x) 0
  (+) (C a b) (C c d) = C (a+c) (b+d)
  (*) (C a b) (C c d) = C (a*c-b*d) (a*d + b*c)
  (-) (C a b) (C c d) = C (a-c) (b-d)
  abs cc = C (sqrt.real $ cc * conj cc) 0

instance Fractional Complex where
  (/) cc dd = cc * conj dd
  recip cc = conj cc
  fromRational x = C (fromRational x) 0 

-- ‘pi’, ‘exp’, ‘log’, ‘sin’, ‘cos’, ‘asin’, ‘acos’, ‘atan’, ‘sinh’,
-- ‘cosh’, ‘asinh’, ‘acosh’, and ‘atanh’
instance Floating Complex where
  sqrt (C a b) = C (sqrt a) (sqrt b)