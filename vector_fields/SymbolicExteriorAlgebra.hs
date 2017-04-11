module SymbolicExteriorAlgebra where
import Prelude hiding ((^))

data Ext f s = Rval f String |
               Del (Ext f String) |
               Wedge (Ext f String) (Ext f String)

type V1 = Ext (Integer->Integer) String
type V2 = Ext (Integer->Integer->Integer) String

instance Show (Ext f s) where
  show (Wedge dx dy) = show dx ++ " ^ " ++ show dy
  show (Del dx) = "d" ++ show dx
  show (Rval f s) = s

df1, df2 :: V1
df1 = Del $ Rval (\n -> 1) "f1"
df2 = Del $ Rval (\n -> n*2) "f2"
wedge = Wedge df1 df2

df3 :: V2
df3 = Del $ Rval (\x y -> x*2) "f3"

{--
f = (x1 .. xn)
df = ∑ ∂i(f) dxi = ∂1(f) dx1 + ... + ∂n(f) dxn
*df = (-1)**(i-1) Σ ∂i(f) dx1 ^...^ dx(i-1) ^ dx(i+1) ^...^ dxn
--}

{--
One of the stumbling blocks here is the desire to have
dependent types. I would like to be able to apply the same
ideas to any scalar-valued function regardless of the
dimension of the functions domain. Perhaps, I can get at
this by developing a type class which demands instances
of summing and product etc...
--}