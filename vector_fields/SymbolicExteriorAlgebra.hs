module SymbolicExteriorAlgebra where
import Prelude hiding ((^))

data Ext f s = Rval f String |
               Del (Ext f String) |
               Wedge (Ext f String) (Ext f String)

type OneForm = Ext (Integer->Integer) String
type TwoForm = Ext (Integer->Integer->Integer) String

instance Show (Ext f s) where
  show (Wedge dx dy) = show dx ++ " ^ " ++ show dy
  show (Del dx) = "d" ++ show dx
  show (Rval f s) = s

df1, df2 :: OneForm
df1 = Del $ Rval (\n -> 1) "f1"
df2 = Del $ Rval (\n -> n*2) "f2"
wedge = Wedge df1 df2

df3 :: TwoForm
df3 = Del $ Rval (\x y -> x*2) "f3"

{--
f = (x1 .. xn)
df = ∑ ∂i(f) dxi = ∂1(f) dx1 + ... + ∂n(f) dxn
*df = (-1)**(i-1) Σ ∂i(f) dx1 ^...^ dx(i-1) ^ dx(i+1) ^...^ dxn
--}