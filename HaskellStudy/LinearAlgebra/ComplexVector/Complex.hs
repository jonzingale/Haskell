-- Btw, there exists Data.Complex with its (a :+ b) notation

module Complex where
import Text.Printf

data Complex = C {real::Double, imag::Double} deriving (Eq)

instance Show Complex where
  show (C a b) | b < 0 = printf "%f%fi" a b
               | otherwise = printf "%f+%fi" a b

c1 :: Complex
c1  = C 3 1

conj :: Complex -> Complex
conj (C a b) = C a (-b)

(<|>) :: Complex -> Complex -> Complex
(<|>) a b = a * conj b


instance Num Complex where
  fromInteger x = C (fromInteger x) 0
  (+) (C a b) (C c d) = C (a+c) (b+d)
  (*) (C a b) (C c d) = C (a*c-b*d) (a*d + b*c)
  (-) (C a b) (C c d) = C (a-c) (b-d)
  abs cc = C (sqrt.real $ cc <|> cc) 0
  signum (C a b) | and [a>0, b>0] = 1
                 | and [a>0, b<0] = 4
                 | and [a<0, b>0] = 2
                 | otherwise = 3