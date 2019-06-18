{-# OPTIONS_GHC -Wno-missing-methods #-}

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{--
Here I am attempting to include Characters
in the Abelian class. They should technically
be considered a type dependent on Abelian
instances.

See chars method below for the difficulty.
--}

module Fourier where
import qualified Data.Vector.Unboxed as U
import Data.Complex

em1 = Zn 2 6
chi0 = Chi (\a -> eval $ a * Zn 0 (ord a)) -- evaluates to 1 for all g
chi1 = Chi (\a -> eval $ a * Zn 1 (ord a))
chi2 = Chi (\a -> eval $ a * Zn 2 (ord a))

data Cyclic = Zn Int Int
type C = Complex Float

instance Show Cyclic where
  show (Zn k m) = show (k `mod` m) ++ " + " ++ show m ++ "Z"

instance Eq Cyclic where
  (==) (Zn k m) (Zn l n) = k == l && m == n

instance Num Cyclic where
  (+) (Zn k m) (Zn l n) = Zn (mod (k+l) m) m
  (*) (Zn k m) (Zn l n) = Zn (mod (k*l) m) m
  negate (Zn k m) = Zn (-k) m

class Num a => Abelian a where
  data Chi a :: *

  (<||>) :: Chi a -> a -> C
  (<|>) :: Chi a -> Chi a -> a -> C
  elems :: a -> [a]
  chars :: a -> [Chi a]
  dual :: a -> Chi a
  ord :: a -> Int
  eval :: a -> C
  inv :: a -> a
  idG :: a -> a
  gen :: a -> a
  chs :: a -> [Chi a]

  {--
  Not sure what to do here. It is possible that the
  instance below is what determines the function type,
  and it needs to be more generally defined at this point.
  Data kinds perhaps?
  --}
  -- chs a = [Chi (\g -> eval $ g * i) | i <- elems a]
  -- chs a = [Chi $ (\g -> 1 :+ 0)] -- | i <- elems a]


  elems a = take (ord a) $ iterate (+ (gen a)) $ idG a
  inv a = negate a
  idG a = a - a

approx :: C -> C
approx n | abs(imagPart n) < eball && abs(realPart n) < eball = 0 :+ 0
         | abs(imagPart n) > eball && abs(realPart n) < eball = 0 :+ imagPart n
         | abs(imagPart n) < eball = realPart n :+ 0
         | otherwise = n
  where eball = 0.0001

instance Abelian Cyclic where
  data Chi Cyclic = Chi (Cyclic -> C)
  chars a = [Chi (\g -> eval $ g * i) | i <- elems a]

  (<||>) (Chi f) (Zn k m) = approx $ f (Zn k m)
  (<|>) (Chi f) (Chi h) g = (approx.sum) [(f.inv) gi * h gi | gi <- elems g]
  dual (Zn a m) = Chi $ \j -> eval $ j * (Zn a m)
  gen (Zn a m) = Zn 1 m
  ord (Zn x m) = m

  eval (Zn x m) =
    let ratio = fromIntegral x / fromIntegral m in
    approx $ exp $ 2 * pi * (0 :+ 1) * ratio

-- test functions
test_chars = zipWith (<||>) (chars em1) (elems em1) -- roots unity
test_principle = [chi0 <||> e | e <- elems em1] -- all 1s

test_sum_of_nontrivial_char = sum [ chi1 <||> g | g <- elems em1] -- should be ZERO
test_conj_inv = [(conjugate $ x <||> em1) * (x <||> em1) | x <- chars em1] -- all 1s

table elem = do -- the character table
  let rows = [x <||> e | e<- elems elem, x <- chars elem]
  putStr.unlines $ f rows
  where
    f [] = []
    f rs = ((++ "\n").show.(take 6) $ rs) : f (drop 6 rs)

verify_orthogonality_of_chars = do -- diagonals are ord em1, 0 otherwise
  let rows = [(x1 <|> x2) em1 | x1<- chars em1, x2 <- chars em1]
  putStr.unlines $ f rows
  where
    f [] = []
    f rs = ((++ "\n").show.(take 6) $ rs) : f (drop 6 rs)
