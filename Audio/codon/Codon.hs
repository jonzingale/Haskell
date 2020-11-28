module Codon where

data Codon =
  UAA | UAG | UGA | -- Stop
  AUG | -- Methionine, start
  UUU | UUC | -- Phenylalanine
  UUA | UUG | CUU | CUC | CUA | CUG | -- Leucine
  AUU | AUC | AUA | -- Isoleucine
  GUU | GUC | GUA | GUG | -- Valine
  UCU | UCC | UCA | UCG | AGU | AGC | -- Serine
  CCU | CCC | CCA | CCG | -- Proline
  ACU | ACC | ACA | ACG | -- Threonine
  GCU | GCC | GCA | GCG | -- Alanine
  UAU | UAC | -- Tyrosine
  CAU | CAC | --  Histidine
  CAA | CAG | -- Glutamine
  AAU | AAC | -- Asparagine
  AAA | AAG | -- Lysine
  GAU | GAC | -- Aspartic
  GAA | GAG | -- Glutamic
  UGU | UGC | -- Cysteine
  UGG | -- Tryptophan
  CGU | CGC | CGA | CGG | AGA | AGG | -- Arginine
  GGU | GGC | GGA | GGG -- Glycine
  deriving (Show, Eq)