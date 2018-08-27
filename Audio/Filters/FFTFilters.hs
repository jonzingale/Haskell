-- http://hackage.haskell.org/package/pure-fft-0.2.0/docs/Numeric-FFT.html
module Filters.FFTFilters where
import qualified Data.Vector.Unboxed as U
import Numeric.FFT (fft, ifft)
import Data.Int (Int32)
import Data.Complex

type FilterKernel = U.Vector (Complex Double)
type VectSamples = U.Vector Int32
type CutOffFreq = Complex Double
type Q = Complex Double

{--
FFT Transform both inport x[n] and impulse response h[n].
Multiply the two transforms. Invert FFT to derive the
convolved inputs. Note that Length of lists must be 2^a.
--}

mm = 2^18::Complex Double -- power of 2

blackman j m = 0.42 - 0.50*cos(2*pi*j/m) + 0.08*cos(4*pi*j/m)
hamming j m  = 0.54 - 0.46*cos(2*pi*j/m)

sinc f j m | j == m/2 = 2*pi*f/44100
           | otherwise = sin(2*pi*f/44100 * (j-m/2)) / (j-m/2)

fftHighPass :: CutOffFreq -> VectSamples -> VectSamples
fftHighPass fc ss = convolve ss (specInv.hh $ fc)

fftBandPass :: Q -> CutOffFreq -> VectSamples -> VectSamples
fftBandPass q freq vs =
  let 
    lp = hh $ freq - q/2
    hp = specInv.hh $ freq + q/2
    bp = specInv $ U.zipWith (+) lp hp
  in convolve vs bp

fftLowPass :: CutOffFreq -> VectSamples -> VectSamples
fftLowPass fc ss = convolve ss (hh fc)

convolve :: VectSamples -> FilterKernel -> VectSamples
convolve xs hs =
  let xx = (U.map fromIntegral xs)::FilterKernel
      [x, h] = map (fft.(U.toList)) [xx, hs] 
      c = ifft $ zipWith (*) x h
  in U.fromList $ map (floor.realPart) c

specInv :: FilterKernel -> FilterKernel
specInv h =
  let m = div (floor.realPart $ mm) 2
      ih = U.map negate h
  in (U.//) ih [(m, (U.!) ih m + 1)]

hh :: CutOffFreq -> FilterKernel
hh fc = normalize $ U.generate (floor.realPart $ mm) (g.fromIntegral)
  where
    normalize h = U.map (/ (U.sum h)) h
    g j = (sinc fc j mm) * blackman j mm