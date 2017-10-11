module Numbers where 

data Surreal a = S (Surreal a) (Surreal a)
                | E | L a | R a deriving (Show, Eq)

left :: Fractional a => Surreal a -> a
left (S E _) = 0
left (S (L l) r) = l

right :: Fractional a => Surreal a -> a
right (S _ E) = 0
right (S l (R r)) = r

ii :: (Eq a , Ord a, Fractional a) => a -> Surreal a
ii x | x == 0 = S E E
     | x > 0 = S (L (x-1)) E
     | otherwise = S E (R (-(x+1)))

-- rho :: Fractional a => Surreal a  -> a

surN = S (L (1/2)) (R 1)

{--
I need a notion of least form to
compare two surreal numbers. The OG
definition has mixed types and relies
on abstraction to a large degree:

'We say x >= y iff (no xR <= y and
x <= no yL).'

I am not sure whether to think of y
as the number itself or its representation.
--}

-- instance (Eq a) => Eq (Surreal a) where
--   (==) x y = True
--   -- (==) (S a b) (S c d) = and [a == c, b == d]

-- instance (Fractional a, Ord a) => Ord (Surreal a) where
--   (>=) a b | right a > rho b = True
--                       | otherwise = False 

-- 1/2 :: Fractional a => a
-- *Numbers> (1/2) < (2/3)

-- rho :: Surreal -> Surreal
-- rho (S a b) =   


-- instance Surreal Q where
  -- (Z a) + (Z b) = Z $ (a + b) `mod` 10
  -- fromInteger x = Z $ (fromInteger x) `mod` 10
  -- (Z a) * (Z b) = Z $ (a * b) `mod` 10
  -- negate (Z a) = Z $ (- a) `mod` 10
  -- signum _ = 1
  -- abs a = a
