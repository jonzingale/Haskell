{--
1. Type Classes: Ord, Eq
    ♣ Clubs ♦ Diamonds ♥ Hearts ♠ Spades
2. Lists:
    - Induction <-> Recursion
    a) base - space
    b) sum(n), triangle(n)
    c) take 12 $ map (* 0.5) S

    - map, take, drop, filter
3. qbot
qbot :: Ord a => [a] -> [a]
qbot [] = []
qbot (x:xs) = qbot (lt x xs) ++ [x] ++ qbot (gte x xs)
  where
    lt x xs = filter (< x) xs
    gte x xs = filter (>= x) xs
--}

-- data Suit = ♣ | ♦ | ♥ | ♠
data Suit = Club | Die | Hart | Spay deriving(Show, Eq) 

data Card = C (Int, Suit) deriving (Show, Eq)

instance Ord Card where
  compare (C (i, _)) (C (j, _))
    | i < j = GT
    | otherwise = LT

hand :: [Card]
hand = [
    C (7, Die),
    C (9, Hart),
    C (10, Club),
    C (4, Club),
    C (3, Die),
    C (10, Die)]

value :: Card -> Int
value (C (i, s)) = i

suit :: Card -> Suit
suit (C (i, s)) = s

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort (lt x xs)) ++ [x] ++ (qsort (gte x xs))
  where
    lt x xs = filter (> x) xs
    gte x xs = filter (<= x) xs








