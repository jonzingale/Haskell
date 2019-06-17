{-# OPTIONS_GHC -Wno-missing-methods #-}

module Fourier where
import qualified Data.Vector.Unboxed as U
import Data.Complex

type C = Complex Float

em1 = Zn 2 6
chi0 = Chi (\a -> eval $ Zn 0 (ord a)) -- evaluates to 1 for all g
chi1 = Chi (\a -> eval a)
chi2 = Chi (\a -> eval $ a + (gen a) + (gen a))

data Cyclic = Zn Int Int | Chi ( Cyclic -> C ) -- k + mZ

instance Show Cyclic where
  show (Zn k m) = show (k `mod` m) ++ " + " ++ show m ++ "Z"
  show (Chi f) = "χ()"-- work this out

instance Num Cyclic where
  (+) (Zn k m) (Zn l n) = Zn (mod (k+l) m) m
  (+) (Chi f) (Chi g) = Chi (\a -> (f a) * (g a))
  negate (Zn k m) = Zn (-k) m
  negate (Chi f) = Chi $ f . negate

class Num a => Abelian a where
  (<||>) :: a -> a -> C
  (<|>) :: a -> a -> a -> C
  chars :: a -> [a]
  char :: a -> a -> C
  elems :: a -> [a]
  ord :: a -> Int
  eval :: a -> C
  dual :: a -> a
  inv :: a -> a
  idG :: a -> a
  gen :: a -> a

  elems a = take (ord a) $ iterate (+ (gen a)) $ idG a
  char i = \j -> eval (i+j)
  inv a = negate a
  idG a = a - a

instance Abelian Cyclic where
  chars j = map (Chi . (\(Zn e n) (Zn a m) -> eval(Zn (e*a) m) )) $ elems j

  (<||>) (Chi f) (Zn k m) = f (Zn k m) -- verify not conjugate
  -- (<||>) (Chi f) (Chi h) | f == h = ord domain
  --                        | otherwise = 0 -- intended othogonality

  (<|>) (Chi f) (Chi h) g = sum [(conjugate.f) gi * h gi | gi <- elems g]
  eval (Zn x m) = -- alternatively chi0 <||> (Zn x m)
    let ratio = fromIntegral x / fromIntegral m in
    f $ exp $ 2 * pi * (0 :+ 1) * ratio
    where
      f n | abs(imagPart n) < 0.01 = realPart n :+ 0
          | otherwise = n

  gen (Zn a m) = Zn 1 m
  ord (Zn x m) = m


tt f h g = [(conjugate (f <||> gi)) * (h <||> gi) | gi <- elems g]

-- test functions
test_chars = zipWith (<||>) (chars em1) (elems em1) -- roots unity
test_inner1 = [chi0 <||> e | e <- elems em1] -- all 1s

test_sum_of_nontrivial_char = sum [ chi1 <||> g | g <- elems em1] -- should be ZERO
test_conj_inv = [(conjugate $ x <||> em1) * (x <||> em1) | x <- chars em1] -- all 1s

verify_orthogonality_of_chars = [( chi2 <|> chi1) zn | zn <- elems em1] --  1 if the same, 0 otherwise

table elem = do
  let rows = [x <||> e | e<- elems elem, x <- chars elem]
  putStr.unlines $ f rows
  where
    f [] = []
    f rs = ((++ "\n").show.(take 6) $ rs) : f (drop 6 rs)
