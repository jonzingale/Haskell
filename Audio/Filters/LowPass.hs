{--
http://www.analog.com/media/en/technical-documentation/dsp-book/dsp_book_Ch16.pdf
--}
module Filters.LowPass where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)

type SamplesR = U.Vector Double
type VectSamples = U.Vector Int32

fc = 882/44100 -- cutoff frequency 882 hz
(mm, mm') = (100::Int, 100::Double)

blackman v j m = (* v) $ 0.42 - 0.5*cos(2*pi*j/m) + 0.08*cos(4*pi*j/m)
hamming v j m  = (* v) $ 0.54 - 0.46*cos(2*pi*j/m)

kerh :: SamplesR
kerh = normalize $ U.generate (mm+1) (g.fromIntegral)
  where
    normalize h = U.map (/ (U.sum h)) h

    g j | j == mm'/ 2 = blackman (2*pi*fc) j mm'
        | otherwise =
          let val = sin(2*pi*fc * (j-mm'/2)) / (j-mm'/2) in
          blackman val j mm'
          -- hamming val j mm'

lowPass :: VectSamples -> VectSamples
lowPass samples = -- Convolve the input signal & filter kernel
  let xx = (U.map fromIntegral samples)::SamplesR in
  let padx = (U.++) (U.replicate mm (0::Double)) xx in
  U.map floor $ U.drop mm $ U.generate (U.length xx) (f padx kerh)
  where
    f x h j = sum [(U.!) x (j+mm-i) * (U.!) h i | i<-[0..mm]]
