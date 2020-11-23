module AminoAcidToPitch where
import qualified Data.Set as S
import AminoAcid

type Freq = Double
data Pitch = P Int Freq | Rest deriving (Show, Eq, Ord)

pitches = [ P i fq | (i, fq) <- zip [0..] aPitches ]

pp :: Show a => [a] -> IO()
pp = putStr.unlines.(map show)

freq = \n -> 2.0 ** (n/20)
intervals = [ freq i | i <- [0..20.0] ]
aPitches = map (* 110) intervals -- octave of 20Tone @ 110hz
aHarm = [110 * i | i <- [0..50.0]] -- linear all day

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
      | f < 220 = f
      | otherwise = scope (f/2)

freqToPitch :: Freq -> Pitch
freqToPitch f = g (quantize f) pitches
  where
    g f ((P i fq):as)
      | f == fq = P i fq 
      | otherwise = g f as

-- accumulate pitches until set is complete
harmonics :: [Pitch]
harmonics = g [freqToPitch (110.0 * i) | i <- [0..]] S.empty []
  where
    incl a bs = S.union (S.singleton a) bs
    g (h:hs) acc ps
      | S.size acc == 20 = ps
      | S.size acc < S.size (incl h acc) =
        g hs (incl h acc) (ps++[h])
      | otherwise = g hs (incl h acc) ps

-- The correlation here ought to be by abundance of Acid.
acidToPitch :: AminoAcid -> Pitch
acidToPitch acid =
  case acid of
  Stop -> Rest
  Phenylalanine -> harmonics!!0
  Leucine -> harmonics!!1
  Isoleucine -> harmonics!!2
  Methionine -> harmonics!!3 -- Start
  Valine -> harmonics!!4
  Serine -> harmonics!!5
  Proline -> harmonics!!6
  Threonine -> harmonics!!7
  Alanine -> harmonics!!8
  Tyrosine -> harmonics!!9
  Histidine -> harmonics!!10
  Glutamine -> harmonics!!11
  Asparagine -> harmonics!!12
  Lysine -> harmonics!!13
  Aspartic -> harmonics!!14
  Glutamic -> harmonics!!15
  Cysteine -> harmonics!!16
  Tryptophan -> harmonics!!17
  Arginine -> harmonics!!18
  Glycine -> harmonics!!19
