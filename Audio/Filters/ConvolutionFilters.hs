{--
http://www.analog.com/media/en/technical-documentation/dsp-book/dsp_book_Ch16.pdf
--}
module Filters.ConvolutionFilters where
import Prelude hiding (map, length, replicate, drop, sum, zipWith, (++))
import Data.Vector.Unboxed hiding (foldr)
import Data.Int (Int32)

type VectSamples = Vector Int32
type FilterKernel = Vector Double
type CutOffFreq = Double
type Q = Double

mm = 100::Int -- really ought to be 4/bw for vals 0..0.5
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
    bp = specInv $ zipWith (+) lp hp
  in convolve vs bp

convolve :: VectSamples -> FilterKernel -> VectSamples
convolve xs hs = 
  let padx = padInput xs
  in cc (length padx) (conKer padx hs)
  where
    cc l v = map floor $ drop mm $ generate l v
    conKer x h j = foldr (+) 0 [(!) x (j+mm-i) * (!) h i | i<-[0..mm]]
    padInput vs = (++) (replicate mm (0::Double)) (map fromIntegral vs)

specInv :: FilterKernel -> FilterKernel
specInv h = let (m, ih) = (div mm 2, map negate h) in
  (//) ih [(m, (!) ih m + 1)]

hh :: CutOffFreq -> FilterKernel
hh fc = normalize $ generate (mm+1) (g.fromIntegral)
  where
    normalize h = map (/ (sum h)) h
    g j = (sinc fc j (fromIntegral mm)) * blackman j (fromIntegral mm)