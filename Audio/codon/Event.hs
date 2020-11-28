module Event where
import AminoAcidToPitch
import AminoAcid
import Codon

data ChemEvent = Event {
  codon :: Codon,
  bases :: String,
  aminoAcid :: AminoAcid
} deriving (Show, Eq)

data Duration = Eighth | Quarter | Half | Whole
data HarmonicDistribution = Harmonics [Double]
data ThirdBase = Hmm

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