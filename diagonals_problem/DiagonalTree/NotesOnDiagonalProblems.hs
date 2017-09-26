module NotesOnDiagonalProblems where
import System.Random

-- ################ work in progress
    --- psuedoprimes
mkBlanket :: Int -> StdGen
mkBlanket cozy = mkStdGen cozy

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


{-- Note1
These heuristics show that on Average,
I should expect to make a reversal once
the height of the tree is 6.
--}
avg :: [Int] -> Int
avg as = div (foldr (+) 0 as) (length as)

randos :: Int -> Int
randos seed = ff 0 0 0 (mkBlanket seed)
  where
    ff 1 2 k s = k
    ff 2 1 k s = k
    ff i j k s = let (a, b) = next s in
      ff j (mod a 3) (k+1) b

{-- Note2
Exponential: Y = a x (b^X) 
Y = 12.792(.1061255^x)

Integrating on my TI-85 from 0 to 20
gives 5.7027, which is only negligibly
better than from 0 to 3

--}

density :: [(Float, (Int, Int))]
density = [(ff k/gg (k-1), hh k) | k<-[1..] ]
  where
    ff j = fromIntegral.tribNum $ j
    gg j = fromIntegral 3^j
    hh j = (ff j, gg (j-1))

{--
Quick calculation of expected Waiting Time
1/(1-x)^2 = Σ(n+1)x^n

goodOnes: [(1,1),(3,3),(7,9), (17,27),  (41,81),    (99,243)]
waitingT: [(1,0),(9,0),(63,4),(459,100),(3321,1600),(24057,20736)

[Inf,Inf,15.75,4.59,2.076,1.160,0.725,0.487,0.342,0.249]
--}
type Q = (Int, Int)

wtExpect :: Q -> Q
wtExpect (n, d) = (d*n, (d-n)^2)

