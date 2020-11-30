module AminoAcidToPitch where
import qualified Data.Set as S
import Data.List (findIndex)
import AminoAcid

tonic :: Double
tonic = 110 * 4

-- Helpers
pp :: Show a => [a] -> IO()
pp = putStr.unlines.(map show)

-- AminoAcidToPitch
type Freq = Double
data Pitch = P Int Freq | Rest deriving (Show, Eq, Ord)

interval :: Pitch -> Int
interval Rest = 0
interval (P i _) = i

frequency :: Pitch -> Freq
frequency Rest = 0
frequency (P _ f) = f

pitches = [ P i fq | (i, fq) <- zip [0..] aPitches ]

freq = \n -> 2.0 ** (n/20)
intervals = [ freq i | i <- [0..20.0] ]
aPitches = map (* tonic) intervals -- octave of 20Tone @ tonic frequency
aHarm = [tonic * i | i <- [0..50.0]] -- linear all day

{--
Quanitizing is effecively a partition function with
a metric that partitions into a tonal system by
approximation. Take a frequency and calculate its
distance from all pitch centers.
--}

quantize :: Freq -> Freq
quantize f = snd.minimum $ 
  [(del p (scope f), p) | p <- aPitches]
  where
    -- simple metric
    del f pf = abs (f - pf)
    -- divide by 2 until within range
    scope f
      | f < 2*tonic = f
      | otherwise = scope (f/2)

freqToPitch :: Freq -> Pitch
freqToPitch f = g (quantize f) pitches
  where
    g f ((P i fq):as)
      | f == fq = P i fq 
      | otherwise = g f as

-- TODO: make this infinite list
-- accumulate pitches until set is complete
harmonics :: [Pitch]
harmonics = g [freqToPitch (tonic * i) | i <- [0..]] S.empty []
  where
    incl a bs = S.union (S.singleton a) bs
    g (h:hs) set pitches
      | S.size set == 20 = pitches
      | S.size set < S.size (incl h set) =
        g hs (incl h set) (pitches++[h])
      | otherwise = g hs (incl h set) pitches

acidToPitch :: AminoAcid -> Pitch
acidToPitch Stop = Rest
acidToPitch acid =
  let Just idx = findIndex (== acid) aminoAcids in harmonics!!idx
