-- http://hackage.haskell.org/package/pure-fft-0.2.0/docs/Numeric-FFT.html
module Filters.FFTFilters where
import qualified Data.Vector.Unboxed as U
import Numeric.FFT (fft, ifft)
import Data.Int (Int32)
import Data.Complex

type VectSamples = U.Vector Int32
type SamplesR = U.Vector Double
type CutOffFreq = Double

mm = 2^18::Double -- power of 2

blackman j m = 0.42 - 0.50*cos(2*pi*j/m) + 0.08*cos(4*pi*j/m)
hamming j m  = 0.54 - 0.46*cos(2*pi*j/m)
sinc f j m | j == m/2 = 2*pi*f/44100
           | otherwise = sin(2*pi*f/44100 * (j-m/2)) / (j-m/2)

{--
FFT Transform both inport x[n] and impulse response h[n].
Multiply the two transforms. Invert FFT to derive the
convolved inputs. Note that Length of lists must be 2^a.
--}

fftFilter :: CutOffFreq -> VectSamples -> VectSamples
fftFilter fc ss =
  let xx = (U.map fromIntegral ss)::SamplesR
      h = fft $ map (:+ 0) $ U.toList (hh fc)
      x = fft $ map (:+ 0) $ U.toList xx
      cc = ifft $ zipWith (*) h x
  in U.map floor $ U.fromList $ map realPart cc

hh :: CutOffFreq -> SamplesR -- kernel
hh fc = normalize $ U.generate (floor mm) (g.fromIntegral)
  where
    normalize h = U.map (/ (U.sum h)) h
    g j = (sinc fc j mm) * blackman j mm