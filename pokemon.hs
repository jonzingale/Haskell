{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
class (Show pokemon, Show (Move pokemon)) => Pokemon pokemon where
  data Move pokemon :: *
  pickMove :: pokemon -> Move pokemon

data Fire = Charmander | Charmeleon | Charizard deriving Show
instance Pokemon Fire where
  data Move Fire = Ember | FlameThrower | FireBlast deriving Show
  pickMove Charmander = Ember
  pickMove Charmeleon = FlameThrower
  pickMove Charizard = FireBlast

data Water = Squirtle | Wartortle | Blastoise deriving Show
instance Pokemon Water where
  data Move Water = Bubble | WaterGun deriving Show
  pickMove Squirtle = Bubble
  pickMove _ = WaterGun

data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show
instance Pokemon Grass where
  data Move Grass = VineWhip deriving Show
  pickMove _ = VineWhip

main :: IO ()
main = do
  print $ pickMove Squirtle
  print $ pickMove Charmander
  print $ pickMove Ivysaur


-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/type-families-and-pokemon