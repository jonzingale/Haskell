module Event where
import AminoAcidToPitch
import AminoAcid
import Peptide
import Codon

data Duration = Eighth | Quarter | Half | Whole deriving (Show, Eq, Ord)
data HarmonicDistribution = Harmonics [Double] deriving (Show)-- oboe, clarinet, ...
data ADSR = Hmm -- strings, piano, ...

type Epoch = Duration

-- consider Nonpolar|Polar|Basic|Acidic for coarser typing than AminoAcid
-- maybe for detemining rest durations? Polar -> "x." versus NonPolar -> "xx"
-- https://en.wikipedia.org/wiki/Genetic_code

class SoundEvent a where
  pitch :: a -> Pitch
  duration :: a -> Duration -- time that event is experienced
  epoch :: a -> Duration -- time before next monophonic event
  harmonics :: a -> HarmonicDistribution
  adsr :: a -> ADSR

instance SoundEvent ChemEvent where
  pitch = acidToPitch.aminoAcid
  duration (Event _ (a:b:c:[]) _) =
    case b of -- order by abundance?
      'a' -> Eighth
      't' -> Quarter
      'g' -> Half
      'c' -> Whole
  epoch (Event _ (a:b:c:[]) _) =
    case c of -- order by abundance?
      'a' -> Eighth
      't' -> Quarter
      'g' -> Half
      'c' -> Whole
  harmonics = undefined -- based on 'a'
  adsr = undefined

--

data ChemEvent = Event {
  codon :: Codon,
  bases :: String,
  aminoAcid :: AminoAcid
} deriving (Show, Eq)

peptideToEvents :: Peptide -> [ChemEvent]
peptideToEvents ps = f.triples $ ps
  where
    f [] = []
    f (s:ss) = Event (fst.toCodonAcid $ s) s (snd.toCodonAcid $ s) : f ss
    triples (a:b:c:cs) = (a:b:c:[]) : triples cs
    triples [] = []

notesDuration :: [(Freq, Duration, Duration)] -- pitch, epoch, duration
notesDuration = [(frequency.pitch $ c, epoch c, duration c) |
  c <- peptideToEvents $ "atgcttagtgcactcacgcagtataattaa"]
