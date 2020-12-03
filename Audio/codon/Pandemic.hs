module Pandemic where
import qualified Data.Vector.Unboxed as U
-- import Wave (VectSamples)
import Data.Int (Int32)
import Peptide
import Event
import Prelude hiding (Left, Right)
import Data.WAVE
import Main
import Wave

{--
Here is where the artistic choices for composition live.

TODO:
- Stringed Instruments: Noise
- long vs. short tones
- frequency ranges
- Harmonics?? maybe in Event?
- Mixer
- parallelized components

Peptide lengths
[4406,2596,1274,420,276,223,122,122,62,25,21,19,17,14,12,11,10,4,2]

Possible Peptide partition
4406,2596,1274, -- fast blips, dense short tones
420,276,223, -- melodies, short tones
122,122,62, -- strings, long tones
25,21,19,17,14,12,11,10,4,2 -- sparse short tones
--}

data Pan = Left | LeftCenter | Center | RightCenter | Right deriving (Show, Eq)

compileStereoSound :: VectSamples -> Pan -> (VectSamples, VectSamples)
compileStereoSound samples pan =
  let volume r = round.(* r).fromIntegral in
  let (volL, volR) = case pan of
                      Left -> (volume 1, \x -> 0)
                      LeftCenter -> (volume 0.25, volume 0.75)
                      Center -> (volume 0.5, volume 0.5)
                      RightCenter -> (volume 0.25, volume 0.75)
                      Right -> (\x -> 0, volume 0.5) in
  let left = U.map volL samples in
  let right = U.map volR samples in
  (left, right)


-- Simple Files
type Tone = (Scalar -> Volume -> Octave -> Sound -> VectSamples)

simpleShortFile :: Int -> Tone -> Double -> IO ()
simpleShortFile n tone oct = do
  datum <- readFile "./covid_cdna.txt"
  let dna = concat.words $ datum
  let peptide = (!! n) $ extractPeptides dna
  let sound = U.concat $ map (tone (scale peptide) 1 oct) $ peptideToSound peptide
  putWAVEFile ("tracks/temp"++show n++".wav") $ pack sound

generateFiles =
  sequence [ simpleShortFile n t o | (n, (t, o)) <- zip [0..18] (zip ts os) ]
  where
    os = [2,5,3,3,1,5,3,0,1,0,3,1,1,1,0,3,2,0,3]
    ts = [shortTones, shortTones, shortTones, shortTones, longTones,
          shortTones, longTones, longTones, longTones, longTones, shortTones,
          shortTones, shortTones, shortTones, longTones, shortTones, longTones,
          shortTones, shortTones]
