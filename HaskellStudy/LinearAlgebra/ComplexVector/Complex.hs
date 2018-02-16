-- Btw, there exists Data.Complex with its (a :+ b) notation
{-# OPTIONS_GHC -Wno-missing-methods #-} -- because of Floating.

module Complex where
import Text.Printf

c1 :: Complex
c1  = C 3 1

data Complex = C {real::Double, imag::Double} deriving (Eq)

instance Show Complex where
  show (C a b) | b < 0 = printf "%f%fi" a b
               | otherwise = printf "%f+%fi" a b

class Comp c where
  conj :: c -> c
  (<|>) :: c -> c -> c

instance Comp Complex where
  conj (C a b) = C a (-b)
  (<|>) a b = a * conj b

instance Num Complex where
  fromInteger x = C (fromInteger x) 0
  (+) (C a b) (C c d) = C (a+c) (b+d)
  (*) (C a b) (C c d) = C (a*c-b*d) (a*d + b*c)
  (-) (C a b) (C c d) = C (a-c) (b-d)
  abs cc = C (sqrt.real $ cc * conj cc) 0
  signum (C a b) | and [a>0, b>0] = 1
                 | and [a>0, b<0] = 4
                 | and [a<0, b>0] = 2
                 | otherwise = 3

instance Fractional Complex where
  (/) cc dd = cc <|> dd
  recip cc = conj cc
  fromRational x = C (fromRational x) 0 

-- ‘pi’, ‘exp’, ‘log’, ‘sin’, ‘cos’, ‘asin’, ‘acos’, ‘atan’, ‘sinh’,
-- ‘cosh’, ‘asinh’, ‘acosh’, and ‘atanh’
instance Floating Complex where
  sqrt (C a b) = C (sqrt a) (sqrt b)