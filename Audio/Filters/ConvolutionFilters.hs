{--
http://www.analog.com/media/en/technical-documentation/dsp-book/dsp_book_Ch16.pdf
--}
module Filters.ConvolutionFilters where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)

type VectSamples = U.Vector Int32
type FilterKernel = U.Vector Double
type CutOffFreq = Double
type Q = Double

(mm, mm') = (100::Int, 100::Double)

blackman j m = 0.42 - 0.50*cos(2*pi*j/m) + 0.08*cos(4*pi*j/m)
hamming j m  = 0.54 - 0.46*cos(2*pi*j/m)
sinc f j m | j == m/2 = 2*pi*f/44100
           | otherwise = sin(2*pi*f/44100 * (j-m/2)) / (j-m/2)

lowPass :: CutOffFreq -> VectSamples -> VectSamples
lowPass fc vs = convolve vs (hh fc)

highPass :: CutOffFreq -> VectSamples -> VectSamples
highPass fc vs = convolve vs (specInv.hh $ fc)

bandPass :: Q -> CutOffFreq -> VectSamples -> VectSamples
bandPass q freq vs =
  let 
    lp = hh $ freq - q/2
    hp = specInv.hh $ freq + q/2
    bp = specInv $ U.zipWith (+) lp hp
  in convolve vs bp

convolve :: VectSamples -> FilterKernel -> VectSamples
convolve xs hs = 
  let padx = padInput xs
      lenx = U.length padx
      convolved = U.generate lenx (conKer padx hs)
  in U.map floor $ U.drop mm $ convolved
  where
    conKer x h j = sum [(U.!) x (j+mm-i) * (U.!) h i | i<-[0..mm]]
    padInput vs = let xx = U.map fromIntegral vs in
      (U.++) (U.replicate mm (0::Double)) xx

specInv :: FilterKernel -> FilterKernel
specInv h =
  let m = div mm 2
      ih = U.map negate h
  in (U.//) ih [(m, (U.!) ih m + 1)]

hh :: CutOffFreq -> FilterKernel
hh fc = normalize $ U.generate (mm+1) (g.fromIntegral)
  where
    normalize h = U.map (/ (U.sum h)) h
    g j = (sinc fc j mm') * blackman j mm'