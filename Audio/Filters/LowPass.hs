module Filters.LowPass where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import System.Random

type SamplesR = U.Vector Double
type VectSamples = U.Vector Int32
type Frequency = Double
type Q = Double

randos :: VectSamples
randos = (U.fromList).(take 44100) $ rs
  where rs = randomRs (minBound, maxBound::Int32) $ mkStdGen 23

samplingRate = 44100; -- /* sample rate in samples per second */

{--
http://www.dspguide.com/filtexam.htm

100 'LOW-PASS WINDOWED-SINC FILTER
110 'This program filters 5000 samples with a 101 point windowed-sinc filter,
120 'resulting in 4900 samples of filtered data.
130 '
140 DIM X[4999] 'X[ ] holds the input signal
150 DIM Y[4999] 'Y[ ] holds the output signal
160 DIM H[100] 'H[ ] holds the filter kernel
170 '
180 PI = 3.14159265
190 FC = .14 'Set the cutoff frequency (between 0 and 0.5)
200 M% = 100 'Set filter length (101 points)
210 '
220 GOSUB XXXX 'Mythical subroutine to load X[ ]
230 '
240 ' 'Calculate the low-pass filter kernel via Eq. 16-4
250 FOR I% = 0 TO 100
260 IF (I%-M%/2) = 0 THEN H[I%] = 2*PI*FC
270 IF (I%-M%/2) <> 0 THEN H[I%] = SIN(2*PI*FC * (I%-M%/2)) / (I%-M%/2)
280 H[I%] = H[I%] * (0.54 - 0.46*COS(2*PI*I%/M%) )
290 NEXT I%
300 '
310 SUM = 0 'Normalize the low-pass filter kernel for
320 FOR I% = 0 TO 100 'unity gain at DC
330 SUM = SUM + H[I%]
340 NEXT I%
350 '
360 FOR I% = 0 TO 100
370 H[I%] = H[I%] / SUM
380 NEXT I%
390 '
400 FOR J% = 100 TO 4999 'Convolve the input signal & filter kernel
410 Y[J%] = 0
420 FOR I% = 0 TO 100
430 Y[J%] = Y[J%] + X[J%-I%] * H[I%]
440 NEXT I%
450 NEXT J%
460 '
470 END
--}

-- U.take 8 $ U.drop 100 $ U.zipWith (-) randos (lowPass randos)

-- fc = 0.1 -- cutoff frequency (0.1 of the sampling rate)
fc = 0.001
(mm, mm') = (100::Int, 100::Double)
hh = U.replicate (mm+1) (0::Double) -- empty filter kernel

fKernel :: SamplesR -> SamplesR
fKernel hs = normalize $ f hs (0::Int) (0::Double)
  where
    normalize h = U.map (/ (U.sum h)) h
    -- g v j = v *  (0.54 - 0.46*cos(2*pi*j/mm')) -- Hamming Window
    g v j = v *  (0.42 - 0.5*cos(2*pi*j/mm') + 0.08*cos(4*pi*j/mm')) -- Blackman Window
    f h i j | i == mm = h
            | otherwise =
      let val = if i == div mm 2
                then 2*pi*fc
                else sin(2*pi*fc * (j-mm'/2)) / (j-mm'/2) in

      f (U.map (g val) h) (i+1) (j+1)

lowPass :: VectSamples -> VectSamples
lowPass samples =
  let xx = (U.map fromIntegral samples)::SamplesR in
  let yy = xx in -- output vector

  U.map floor (f xx yy (fKernel hh) mm) -- j starts at 100
  where
    f x y h j | j == U.length x = y
              | otherwise = 
                let ups = [(j, 100*(U.!) x (j-i) * (U.!) h i) | i<-[0..100]] in
                -- let ups = [(j, (U.!) y j + (U.!) x (j-i) * (U.!) h i) | i<-[0..100]] in
                f x ((U.//) y ups) h (j+1)

    --           | otherwise = f x (g x y h j 0) h (j+1)
    -- g x y h j 10 = y
    -- g x y h j i =
    --   let val = [(j, (U.!) y j + (U.!) x (j-i) * (U.!) h i)] in
    --   g x ((U.//) y val) h j (i+1)

