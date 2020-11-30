module AminoAcid where

 -- ordered by abundance
data AminoAcid =
  Phenylalanine |
  Methionine | -- Start
  Proline |
  Valine |
  Threonine |
  Arginine |
  Asparagine |
  Glutamine |
  Leucine |
  Lysine |
  Alanine |
  Aspartic |
  Serine |
  Histidine |
  Tryptophan |
  Glutamic |
  Isoleucine |
  Tyrosine |
  Cysteine |
  Glycine |
  Stop
  deriving (Show, Eq, Ord, Enum)

aminoAcids = enumFromTo Phenylalanine Glycine -- ordered by abundance
