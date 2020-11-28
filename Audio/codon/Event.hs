module Event where
import AminoAcidToPitch
import AminoAcid
import Peptide
import Codon

class SoundEvent a where
  pitch :: a -> Pitch
  duration :: a -> Duration
  thirdBase :: a -> ThirdBase
  harmonics :: a -> HarmonicDistribution

instance SoundEvent ChemEvent where
  pitch ev = undefined -- aminoAcid ev
  duration (Event _ (a:b:c:[]) _) =
    case a of -- order by abundance?
      'a' -> Eighth
      't' -> Quarter
      'g' -> Half
      'c' -> Whole
  harmonics = undefined
  thirdBase = undefined

--

data ChemEvent = Event {
  codon :: Codon,
  bases :: String,
  aminoAcid :: AminoAcid
} deriving (Show, Eq)

data Duration = Eighth | Quarter | Half | Whole
data HarmonicDistribution = Harmonics [Double]
data ThirdBase = Hmm

peptideToEvents :: Peptide -> [ChemEvent]
peptideToEvents ps = f.triples $ ps
  where
    f [] = []
    f (s:ss) = Event (fst.toCodonAcid $ s) s (snd.toCodonAcid $ s) : f ss

triples :: Peptide -> [String]
triples (a:b:c:cs) = (a:b:c:[]) : triples cs
triples [] = []
