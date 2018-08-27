-- http://hackage.haskell.org/package/pure-fft-0.2.0/docs/Numeric-FFT.html
module Filters.FFTFilters where
import qualified Data.Vector.Unboxed as U
import Numeric.FFT (fft, ifft)
import Data.Int (Int32)
import Data.Complex

type VectSamples = U.Vector Int32
type SamplesC = U.Vector (Complex Double)
type CutOffFreq = Complex Double
type Q = Complex Double

mm = 2^18::Complex Double -- power of 2

blackman j m = 0.42 - 0.50*cos(2*pi*j/m) + 0.08*cos(4*pi*j/m)
hamming j m  = 0.54 - 0.46*cos(2*pi*j/m)
sinc f j m | j == m/2 = 2*pi*f/44100
           | otherwise = sin(2*pi*f/44100 * (j-m/2)) / (j-m/2)

{--
FFT Transform both inport x[n] and impulse response h[n].
Multiply the two transforms. Invert FFT to derive the
convolved inputs. Note that Length of lists must be 2^a.
--}

fftHighPass :: CutOffFreq -> VectSamples -> VectSamples
fftHighPass fc ss =
  let xx = (U.map fromIntegral ss)::SamplesC
      h = fft $ U.toList $ specInv (hh fc)
      x = fft $ U.toList xx
      cc = ifft $ zipWith (*) h x
  in U.map floor $ U.fromList $ map realPart cc

-- not quite right. spectral inversions on H[n]
fftBandPass :: Q -> CutOffFreq -> VectSamples -> VectSamples
fftBandPass q freq ss =
  let (low, hi) = (freq - q/2, freq + q/2)
      lp = fftLowPass low ss
      hp = fftHighPass hi ss
      bandBlock = U.zipWith (+) lp hp
  in specInv bandBlock

fftLowPass :: CutOffFreq -> VectSamples -> VectSamples
fftLowPass fc ss =
  let xx = (U.map fromIntegral ss)::SamplesC
      h = fft $ U.toList (hh fc)
      x = fft $ U.toList xx
      cc = ifft $ zipWith (*) h x
  in U.map floor $ U.fromList $ map realPart cc

specInv :: (U.Unbox b, Num b) => U.Vector b -> U.Vector b
specInv ss = U.map negate ss

hh :: CutOffFreq -> SamplesC -- kernel
hh fc = normalize $ U.generate (floor.realPart $ mm) (g.fromIntegral)
  where
    normalize h = U.map (/ (U.sum h)) h
    g j = (sinc fc j mm) * blackman j mm