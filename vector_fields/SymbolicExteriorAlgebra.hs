module SymbolicExteriorAlgebra where
import Text.Regex
{--
f = (x1 .. xn)
df = ∑ ∂i(f) dxi = ∂1(f) dx1 + ... + ∂n(f) dxn
*df = (-1)**(i-1) Σ ∂i(f) dx1 ^...^ dx(i-1) ^ dx(i+1) ^...^ dxn

I am looking to compute exterior algebra
all the while acting on strings. My thinking is that
for now, Forms will be defined in terms of R^3.
--}

{--
  Todo: incorporate the work on LinearAlgebra
  here so that functions in Ext are extended to Num.
--}


df1, df2, df0 :: V3
df0 = Rval (\i j k -> 1) "f(x y z)"
df1 = Del $ Rval (\i j k -> 1) "g(x y z) dy"
df2 = del df0 "x"
we1 = Wedge df1 df2

-- the Vn refers to the number of arguments in the function.
type V3 = Ext (Integer -> Integer -> Integer -> Integer) String

data Ext f s = Rval f String | Zero |
               Del (Ext f String) |
               Wedge (Ext f String) (Ext f String)

toSym :: Ext f String -> String
toSym (Rval f s) = s
toSym (Del f) = toSym f
toSym (Wedge f g) = (toSym f) ++ " + " ++ toSym g

-- array here is gnar.
toFun :: Ext f String -> [f]
toFun (Rval f s) = [f]
toFun (Del f) = toFun f
toFun (Wedge f g) = (toFun f) ++ toFun g

instance Show (Ext f s) where
  show (Wedge dx dy) = show dx ++ " ^ " ++ show dy
  show (Del dx) = "d" ++ show dx
  show (Rval f s) = s
  show Zero = ""

del :: Ext a String -> String -> Ext a String
del (Rval f s) str = Del (Rval f (s ++ " ^ d" ++ str))
del (Del f) str | match ('d' : str) (toSym f) = Zero
                | otherwise = del f str -- so many freaking special cases.

--- helpers
match :: String -> String -> Bool
match reg str =  matchRegex (mkRegex reg) str == Just []


