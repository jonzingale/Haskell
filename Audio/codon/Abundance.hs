module Abundance where
import Event (peptideToEvents, aminoAcid)
import Peptide (peptides)
import Data.List (findIndex, sortBy)
import Data.Function (on)
import AminoAcid

-- sort acids by abundance: most to least
sortAcids = do
  ps <- peptides
  let acids = map aminoAcid $ concat $ map peptideToEvents ps
  let counts = f acids zeros
  let sorted = sortBy (flip compare `on` snd) $ zip aas counts
  return $ fst.unzip $ sorted -- ordered acids but not counts
  where
    f [] acc = acc
    f (a:as) zs = f as (accumulate a zs)

aas = [
  Stop, Phenylalanine, Leucine, Isoleucine, Methionine, Valine, Serine,
  Proline, Threonine, Alanine, Tyrosine, Histidine, Glutamine, Asparagine,
  Lysine, Aspartic, Glutamic, Cysteine, Tryptophan, Arginine, Glycine]

zeros = take 21 $ repeat 0

accumulate :: AminoAcid -> [Int] -> [Int]
accumulate aa acc =
  let Just idx = findIndex (== aa) aas in
  let sel = (take (idx-1) zeros) ++ [1] ++ zeros in
  zipWith (+) sel acc