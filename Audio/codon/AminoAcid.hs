module AminoAcid where

 -- ordered by abundance
data AminoAcid =
  Leucine |
  Valine |
  Threonine |
  Serine |
  Alanine |
  Glycine |
  Lysine |
  Asparagine |
  Isoleucine |
  Aspartic |
  Phenylalanine |
  Tyrosine |
  Glutamic |
  Proline |
  Glutamine |
  Arginine |
  Cysteine |
  Methionine |
  Histidine |
  Tryptophan |
  Stop
  deriving (Show, Eq, Ord, Enum)

aminoAcids = enumFromTo Leucine Tryptophan
