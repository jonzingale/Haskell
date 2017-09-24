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

tribstr 0 = "0"
tribstr 1 = "1"
tribstr n = tribstr (n-1) ++ tribstr (n-2) ++ tribstr (n-1)

-- Exponential: Y = a x (b^X) 
-- Y = 0.8798(0.9089^X)
tridensity :: [(Float, (Int, Integer))]
tridensity = [((ff (k +1))/(gg k), (ff (k+1), 3^k))| k<-[1..] ]
  where
    ff j = fromIntegral.length.tribstr $ j
    gg j = fromIntegral (3^j)