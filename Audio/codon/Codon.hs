module Codon where
import AminoAcid

data Codon =
  TAA | TAG | TGA | -- Stop
  ATG | -- Methionine, start
  TTT | TTC | -- Phenylalanine
  TTA | TTG | CTT | CTC | CTA | CTG | -- Leucine
  ATT | ATC | ATA | -- Isoleucine
  GTT | GTC | GTA | GTG | -- Valine
  TCT | TCC | TCA | TCG | AGT | AGC | -- Serine
  CCT | CCC | CCA | CCG | -- Proline
  ACT | ACC | ACA | ACG | -- Threonine
  GCT | GCC | GCA | GCG | -- Alanine
  TAT | TAC | -- Tyrosine
  CAT | CAC | --  Histidine
  CAA | CAG | -- Glutamine
  AAT | AAC | -- Asparagine
  AAA | AAG | -- Lysine
  GAT | GAC | -- Aspartic
  GAA | GAG | -- Glutamic
  TGT | TGC | -- Cysteine
  TGG | -- Tryptophan
  CGT | CGC | CGA | CGG | AGA | AGG | -- Arginine
  GGT | GGC | GGA | GGG -- Glycine
  deriving (Show, Eq)

toCodonAcid :: String -> (Codon, AminoAcid)
toCodonAcid (a:b:c:[]) =
  case (a:b:c:[]) of
    "taa" -> (TAA,Stop)
    "tag" -> (TAG,Stop)
    "tga" -> (TGA,Stop)
    "atg" -> (ATG,Methionine)
    "ttt" -> (TTT,Phenylalanine)
    "ttc" -> (TTC,Phenylalanine)
    "tta" -> (TTA,Leucine)
    "ttg" -> (TTG,Leucine)
    "ctt" -> (CTT,Leucine)
    "ctc" -> (CTC,Leucine)
    "cta" -> (CTA,Leucine)
    "ctg" -> (CTG,Leucine)
    "att" -> (ATT,Isoleucine)
    "atc" -> (ATC,Isoleucine)
    "ata" -> (ATA,Isoleucine)
    "gtt" -> (GTT,Valine)
    "gtc" -> (GTC,Valine)
    "gta" -> (GTA,Valine)
    "gtg" -> (GTG,Valine)
    "tct" -> (TCT,Serine)
    "tcc" -> (TCC,Serine)
    "tca" -> (TCA,Serine)
    "tcg" -> (TCG,Serine)
    "agt" -> (AGT,Serine)
    "agc" -> (AGC,Serine)
    "cct" -> (CCT,Proline)
    "ccc" -> (CCC,Proline)
    "cca" -> (CCA,Proline)
    "ccg" -> (CCG,Proline)
    "act" -> (ACT,Threonine)
    "acc" -> (ACC,Threonine)
    "aca" -> (ACA,Threonine)
    "acg" -> (ACG,Threonine)
    "gct" -> (GCT,Alanine)
    "gcc" -> (GCC,Alanine)
    "gca" -> (GCA,Alanine)
    "gcg" -> (GCG,Alanine)
    "tat" -> (TAT,Tyrosine)
    "tac" -> (TAC,Tyrosine)
    "cat" -> (CAT,Histidine)
    "cac" -> (CAC,Histidine)
    "caa" -> (CAA,Glutamine)
    "cag" -> (CAG,Glutamine)
    "aat" -> (AAT,Asparagine)
    "aac" -> (AAC,Asparagine)
    "aaa" -> (AAA,Lysine)
    "aag" -> (AAG,Lysine)
    "gat" -> (GAT,Aspartic)
    "gac" -> (GAC,Aspartic)
    "gaa" -> (GAA,Glutamic)
    "gag" -> (GAG,Glutamic)
    "tgt" -> (TGT,Cysteine)
    "tgc" -> (TGC,Cysteine)
    "tgg" -> (TGG,Tryptophan)
    "cgt" -> (CGT,Arginine)
    "cgc" -> (CGC,Arginine)
    "cga" -> (CGA,Arginine)
    "cgg" -> (CGG,Arginine)
    "aga" -> (AGA,Arginine)
    "agg" -> (AGG,Arginine)
    "ggt" -> (GGT,Glycine)
    "ggc" -> (GGC,Glycine)
    "gga" -> (GGA,Glycine)
    "ggg" -> (GGG,Glycine)


-- sss = ["taa","tag","tga","atg","ttt","ttc","tta","ttg","ctt","ctc","cta","ctg","att","atc","ata","gtt","gtc","gta","gtg","tct","tcc","tca","tcg","agt","agc","cct","ccc","cca","ccg","act","acc","aca","acg","gct","gcc","gca","gcg","tat","tac","cat","cac","caa","cag","aat","aac","aaa","aag","gat","gac","gaa","gag","tgt","tgc","tgg","cgt","cgc","cga","cgg","aga","agg","ggt","ggc","gga","ggg"]
-- ccc = [TAA,TAG,TGA, ATG, TTT,TTC, TTA,TTG,CTT,CTC,CTA,CTG, ATT,ATC,ATA, GTT,GTC,GTA,GTG, TCT,TCC,TCA,TCG,AGT,AGC, CCT,CCC,CCA,CCG, ACT,ACC,ACA,ACG, GCT,GCC,GCA,GCG, TAT,TAC, CAT,CAC, CAA,CAG, AAT,AAC, AAA,AAG, GAT,GAC, GAA,GAG, TGT,TGC, TGG, CGT,CGC,CGA,CGG,AGA,AGG, GGT, GGC, GGA, GGG]