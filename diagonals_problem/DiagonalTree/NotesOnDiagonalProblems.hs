module NotesOnDiagonalProblems where

{--
  Here are some calculations for how
  quickly the good paths fall off. Less than 1% by n == 49
  
  It is also worthwile to note:
  Calculating a waiting times problem for when the first
  12 or 21 pair is likely to happen gives a reasonable
  estimate for why the tree algorithm should do well
  as the length of a string goes to infinity.
-}

tribStr 0 = "0"
tribStr 1 = "1"
tribStr n = tribStr (n-1) ++ tribStr (n-2) ++ tribStr (n-1)

tribNum 0 = 1
tribNum 1 = 1
tribNum n = tribNum (n-1) + tribNum (n-2) + tribNum (n-1)

tribs = [tribNum k | k<-[1..]]

-- Exponential: Y = a x (b^X) 
-- Y = 0.8798(0.9089^X)
density :: [(Float, (Int, Integer))]
density = [(ff k/gg k, hh k) | k<-[1..] ]
  where
    ff j = fromIntegral.tribNum $ j
    gg j = fromIntegral 3^j
    hh j = (ff j, gg (j-1))