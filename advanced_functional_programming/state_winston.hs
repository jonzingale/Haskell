module StateClassExersice where

import System.Random
import Control.Monad
import Control.Monad.State

data Tree a = Unary (Tree a)
            | Binary (Tree a) (Tree a)
            | Ternary (Tree a) (Tree a) (Tree a)
            | A a
            | G a
            | C a
            | T a deriving (Show, Eq)

instance Functor Tree  where
  fmap f tree = case tree of
    A a -> A (f a)
    G a -> G (f a)
    C a -> C (f a)
    T a -> T (f a)
    Ternary a b c -> Ternary (fmap f a) (fmap f b) (fmap f c)
    Binary a b -> Binary (fmap f a) (fmap f b)
    Unary a -> Unary (fmap f a)


foo = Ternary
        (Binary (A ()) (G ()))
        (C ())
        (Ternary
          (G ())
          (G ())
          (Binary (Unary (C ())) (T ())))



label :: Tree a -> Tree Int
label tree = evalState (counter tree) 0
  where counter t = case t of
          A _ -> return (A 0)
          T _ -> return (T 0)
          C _ -> return (C 0)
          G _ -> do
            modify (+1)
            count <- get
            return (G count)
          Unary a       -> Unary <$> counter a
          Binary a b    -> Binary <$> counter a <*> counter b
          Ternary a b c -> Ternary <$> counter a <*> counter b <*> counter c


--- random
getConstructor :: State StdGen (a -> Tree a)
getConstructor = (constructos !!) <$> state (randomR range)
  where constructos = [A, G, C, T]
        range = ( 0, (length constructos) - 1 )


randomise :: Tree a -> Tree a
randomise tree = evalState (mixit tree) (mkStdGen 0)
  where mixit t = case t of
          A x -> do
            c <- getConstructor
            return (c x)
          T x -> do
            c <- getConstructor
            return (c x)
          C x -> do
            c <- getConstructor
            return (c x)
          G x -> do
            c <-getConstructor
            return (c x)
          Unary a -> Unary <$> mixit a
          Binary a b -> Binary <$> mixit a <*> mixit b
          Ternary a b c -> Ternary <$> mixit a <*> mixit b <*> mixit c