--Pythagorean Triples and Monads

module Bird where
type Z = Integer

pyth :: Z->Z->Z->Bool
pyth x y z = x*x+y*y == z*z

triple :: Z -> [(Z,Z,Z)]
triple n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], pyth x y z]

-------State Monad

data Term = Con Z | Div Term Term
newtype St a = MkSt (State -> (a,State))
type State = Z
{--
apply :: St a-> State->(a,State)
apply (MkSt f) s = f s

eval :: Term ->St Z
eval (Con x) = MkSt f
               where f s = (x,s)
eval (Div t u) = MkSt f
                 where 
                f s = (x `div` y, s''+1)
 	           where
			(x,s')  = apply (eval t) s
			(y,s'') = apply (eval u) s'

instance Show a => Show (St a) where
  show f = "value:"++show x++",count:"++show s
	where (x,s) = apply f 0
--}
----

eval :: Monad m => Term -> m Z
eval (Con x)   = return x
eval (Div t u) = do x <- eval t
		    y <- eval u
		    return (x `div` y)

--ID
newtype Id a = MkId a

instance Monad Id where --remember to indent!
 return x = MkId x
 (MkId  x) >>= q = q x
	 --no helpers
      	 --
evalId :: Term -> Id Z
evalId = eval
		--Show
instance Show a => Show (Id a) where
 show (MkId x) = "value:" ++ show x


--State
instance Monad St  where
 return x = MkSt f where f s = (x,s)
 p>>=q    = MkSt f
 	    where
	    f s = apply (q x) s'
 	          where (x,s') = apply p s
	--helpers
tick:: St ()
tick = MkSt f where f s = ((),s+1)

apply :: St a-> State->(a,State)
apply (MkSt f) s = f s
        --

evalSt :: Term -> St Z
evalSt (Con x) = return x
evalSt (Div t u) = do x<-evalSt u
		      y<-evalSt t
		      tick
		      return (x`div`y)
		--Show
instance Show a => Show (St a) where
  show f = "value:"++show x++",count:"++show s
	where (x,s) = apply f 0



