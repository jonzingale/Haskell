module Abundance where
import Event (peptideToEvents, aminoAcid)
import Peptide (Peptide, peptides)
import Data.List (sortBy, partition)
import Data.Function (on)
import AminoAcid

{--
Abundance is a collection of helper functions for calculating
abundances for various data: peptide counts, peptide lengths ...
--}

-- sort acids by abundance: most to least
sortAcids :: IO [AminoAcid]
sortAcids = do
  ps <- peptides
  let acids = map aminoAcid $ concat $ map peptideToEvents ps
  let sorted = sortBy (flip compare `on` snd) $ f acids
  return $ fst.unzip $ sorted -- ordered acids sans counts
  where
    f [] = []
    f (a:as) =
      let (s, r) = partition (== a) (a:as) in
      (a, length s) : f r

sortPeptides :: IO [([AminoAcid], Int)]
sortPeptides = do
    ps <- peptides
    let pepAcids = map (map aminoAcid) $ map peptideToEvents ps
    let psWithLens = [(pep, length pep) | pep <- pepAcids]
    let sorted = sortBy (flip compare `on` snd) psWithLens
    return $ sorted

-- [4406,2596,1274,420,276,223,122,122,62,25,21,19,17,14,12,11,10,4,2]
peptideCounts = do
  ps <- sortPeptides
  return $ map snd ps

