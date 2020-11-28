module Peptide where
import Data.List.Split (splitOn)

type RNA = String
type Peptide = String

-- :set +s

cdna = do
  datum <- readFile "covid_cdna.txt"
  let dna = concat.words $ datum
  return dna

peptides = do
  datum <- readFile "covid_cdna.txt"
  let dna = concat.words $ datum
  return $ extractPeptides dna

extractPeptide :: RNA -> RNA
extractPeptide (a:t:g:zs)
  | (a:t:g:[]) == "atg" = f zs "atg"
  | otherwise = extractPeptide (t:g:zs)
  where
    -- OPTIMIZATION: Better would be to cons then reverse on "gta"
    f (x:y:z:zs) acc
      | any (== [x,y,z]) ["tag", "tga", "taa"] = acc ++ [x,y,z]
      | otherwise = f zs (acc ++ [x,y,z])
    f xs acc = ""

remainingText :: RNA -> RNA
remainingText rna
  | length rna < 3 = ""
  | otherwise = last.splitOn (extractPeptide rna) $ rna

extractPeptides :: RNA -> [Peptide]
extractPeptides [] = []
extractPeptides rna
  | extractPeptide rna == [] = []
  | otherwise = extractPeptide rna : (extractPeptides.remainingText $ rna)
