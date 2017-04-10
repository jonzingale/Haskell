module ExteriorAlgebra where
import Prelude hiding ((^))

-- one list of numbers [1,2,3]
-- another list of chars "dx^dy dy^dz dz^dx"

data Ext n c  = Ext [n] [c] deriving Show
type Form n = Ext Integer Char

form :: Form n
form = Ext [1,2,3] ""

del :: Form n -> Form n
del (Ext ns "") = Ext ns "dx dy dz" -- gen this.

(^) :: Form n -> Form n -> Form n
(^) (Ext (x:xs) "") (Ext (y:ys) "") = Ext xs "dx"


---Helpers
{--
Useful would be methods for string handling.
for instance, given a form (Ext [1..n] ""),
del would give back (Ext [1..n] "d1 ... dn").
--}