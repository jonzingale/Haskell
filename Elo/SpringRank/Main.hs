module Main where
import SpringRank (example)

{--
To compile, run and cleanup:
ghc -O2 -o springrank Main.hs 
time ./springrank
rm *.hi *.o
--}

main = do
  result <- example
  print result
