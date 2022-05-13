module Linear where

-- tensor product
(><) :: Semigroup a => [a] -> [a] -> [a]
(><) as bs = (<>) <$> as <*> bs

-- scalar product
(!*) :: Semigroup a => a -> [a] -> [a]
(!*) a bs = [a] >< bs

(!/) :: (Fractional a, Semigroup a) => [a] -> a -> [a]
(!/) bs a = [1/a] >< bs

instance Semigroup Float where
    (<>) = (*)

instance Num a => Num [a] where
    a + b = zipWith (+) a b -- pairwise addition
    a * b = zipWith (*) a b -- pairwise multiplication
    fromInteger a = [fromInteger a] -- singleton
    negate as = map negate as
    abs as = undefined
    signum = undefined