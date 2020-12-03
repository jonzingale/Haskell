module Main where
import qualified Data.Vector.Unboxed as U
import Filters (highPass, lowPass)
import CompositionHelpers
import Peptide
import Wave

{--
Here is where the artistic choices for composition live.
--}

main' = -- create files
  sequence [ simpleShortFile n t o | (n, (t, o)) <- zip [0..18] (zip ts os) ]
  -- sequence [ simpleShortFile 1 longTones 5, simpleShortFile 5 longTones 5]
  where
    os = [2,5,3,3,1,5,3,0,1,0,3,1,1,1,0,3,2,0,3]
    ts = [shortTones, shortTones, shortTones, shortTones, longTones,
          shortTones, longTones, longTones, longTones, longTones, shortTones,
          shortTones, shortTones, shortTones, longTones, shortTones, longTones,
          shortTones, shortTones]

main = do -- creates composition
  datum <- readFile "./covid_cdna.txt"
  let dna = concat.words $ datum

  -- fast sound : 4406
  let peptide = (!! 1) $ extractPeptides dna
  let k = scale peptide
  let s1 =  U.concat $ map (shortTones k 13 5) $ peptideToSound peptide
  let peptide = (!! 5) $ extractPeptides dna
  let k = scale peptide
  let s6 =  U.concat $ map (shortTones k 13 5) $ peptideToSound peptide
  let s16 = (U.zipWith (+) s1 s6)

  let peptide = (!! 6) $ extractPeptides dna
  let k = scale peptide
  let s7 =  U.concat $ map (longTones k 7 3) $ peptideToSound peptide

  -- avg sound : 420
  let peptide = (!! 14) $ extractPeptides dna
  let s2 =  U.concat $ map toSound $ peptideToSound peptide

  let peptide = (!! 7) $ extractPeptides dna
  let s8 =  U.concat $ map toSound $ peptideToSound peptide

  let peptide = (!! 9) $ extractPeptides dna
  let s9 =  U.concat $ map toSound $ peptideToSound peptide

  -- sparse sounds, 2nd octave? dsec, esec
  let peptide = (!! 0) $ extractPeptides dna
  let k = scale peptide
  let s10 = U.concat $ map (shortTones k 13 2) $ peptideToSound peptide

  let peptide = (!! 2) $ extractPeptides dna
  let k = scale peptide
  let s11 = U.concat $ map (shortTones k 13 3) $ peptideToSound peptide

  let peptide = (!! 3) $ extractPeptides dna
  let k = scale peptide
  let s12 = U.concat $ map (shortTones k 13 3) $ peptideToSound peptide

  let peptide = (!! 15) $ extractPeptides dna
  let k = scale peptide
  let s13 = U.concat $ map (shortTones k 13 3) $ peptideToSound peptide

  let peptide = (!! 18) $ extractPeptides dna
  let k = scale peptide
  let s14 = U.concat $ map (shortTones k 13 4) $ peptideToSound peptide

  let peptide = (!! 10) $ extractPeptides dna
  let k = scale peptide
  let s15 = U.concat $ map (shortTones k 13 3) $ peptideToSound peptide

  -- 11, 12, 13, 17 remaining
  let shorts = U.zipWith (+) s15 $ U.zipWith (+) s14 $ U.zipWith (+) s13 $ U.zipWith (+) s12 $ U.zipWith (+) s10 s11

  -- slow sounds : 25, 17
  let peptide = (!! 4) $ extractPeptides dna
  let k = scale peptide
  let s3 =  U.concat $ map (longTones k 9 1) $ peptideToSound peptide

  let peptide = (!! 8) $ extractPeptides dna
  let k = scale peptide
  let s4 =  U.concat $ map (longTones k 7 1) $ peptideToSound peptide
  let s34 = (U.zipWith (+) s3 s4)

  let peptide = (!! 16) $ extractPeptides dna
  let k = scale peptide
  let s5 =  U.concat $ map (longTones k 12 2) $ peptideToSound peptide
  let s165 = U.zipWith (+) s16 s5

  let sl = U.zipWith (+) shorts $ U.zipWith (+) s9 $ U.zipWith (+) s8 $ U.zipWith (+) s165 s2
  let sr = U.zipWith (+) shorts $ U.zipWith (+) s9 $ U.zipWith (+) s8 $ U.zipWith (+) (U.zipWith (+) s34 s2) s7

  -- mix
  makeStereoWavFile (lowPass 3000 sl) (lowPass 3000 sr)
