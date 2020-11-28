module Event where
import AminoAcidToPitch
import AminoAcid
import Peptide
import Codon

class SoundEvent a where
  pitch :: a -> Pitch
  duration :: a -> Duration
  harmonics :: a -> HarmonicDistribution
  adsr :: a -> ADSR

instance SoundEvent ChemEvent where
  pitch = acidToPitch.aminoAcid
  duration (Event _ (a:b:c:[]) _) =
    case a of -- order by abundance?
      'a' -> Eighth
      't' -> Quarter
      'g' -> Half
      'c' -> Whole
  harmonics = undefined
  adsr = undefined

--

data ChemEvent = Event {
  codon :: Codon,
  bases :: String,
  aminoAcid :: AminoAcid
} deriving (Show, Eq)

data Duration = Eighth | Quarter | Half | Whole deriving (Show)
data HarmonicDistribution = Harmonics [Double] deriving (Show)-- oboe, clarinet, ...
data ADSR = Hmm -- strings, piano, ...

--

peptideToEvents :: Peptide -> [ChemEvent]
peptideToEvents ps = f.triples $ ps
  where
    f [] = []
    f (s:ss) = Event (fst.toCodonAcid $ s) s (snd.toCodonAcid $ s) : f ss
    triples (a:b:c:cs) = (a:b:c:[]) : triples cs
    triples [] = []

pep2Pitch :: [Pitch]
pep2Pitch = map pitch $ peptideToEvents "atgcttagtgcactcacgcagtataattaa"

notesDuration :: [(Freq, Duration)]
notesDuration = [(frequency.pitch $ c, duration c) |
  c <- peptideToEvents $ "atgcttagtgcactcacgcagtataattaa"]
