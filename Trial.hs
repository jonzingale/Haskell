module Trial (maxtrials,toss,tMaxx,coupon,tillepic, sanitycheck) where

import System.Random
import Data.List
import SortsShuffles
import CombProb
import Typehandling

type Apples = Int
type Bins = Int
type Trials = Int
type MaxApples = Int
type Coupons = [Int] 
type Seed = Int
--random array
array n r s = take n $ randomRs(1,r) (mkBlanket s)

---combinatorially modeling balls thrown into bin and returning max bin count
maxtrials :: Apples-> Bins -> Trials -> (Apples,Trials)
maxtrials n r t = (blip.maxfreq.trials n r) t
freq [] = [] --wonderfully modular and useful elsewhere
freq as = ((length.takeWhile (== x))(x:xs),x) : (freq.dropWhile (==x)) xs
	where (x:xs) = rsort as
maxfreq xs = (maximum.freq) xs
trials n r t =  (fst.unzip.map maxfreq) $ take t [array n r s | s<-[1..]] 


-- Give the percentage of total balls expected in the bin with the most
tMaxx n = fromIntegral (n `choose` (n`div`2)) / fromIntegral (2^n)
-- :: Apples->Bins->ExpectedMaxinaBin
toss :: Apples->Bins->MaxApples
toss a b = floor $ fromIntegral a * tMaxx b

----coupon collection
coupon :: Apples -> Trials
coupon n = n * thrufloat log n
tillepic :: Apples -> Seed -> Coupons
tillepic n s = take (coupon n) $ randomRs (1,n) (mkBlanket s)
     --varifiies that coupon produces everybody

sanitycheck :: Coupons -> Bool     
sanitycheck xs = and [x==y|(x,y)<-zip (everybody xs) [1..]]
everybody [] = []
everybody as = x: (everybody.dropWhile (==x)) xs
	where (x:xs) = rsort as


---Helpers
blip (x,y)=(y,x)


{--
The rsort in freq is likely unnecessary as the input is already random
--}

