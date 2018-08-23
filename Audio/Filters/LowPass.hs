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
110 'This program filters 5000 samples with a 101 point windowed-sinc  
120 'filter, resulting in 4900 samples of filtered data.
130 '
140 '                      'INITIALIZE AND DEFINE THE ARRAYS USED
150 DIM X[4999]            'X[ ] holds the input signal
160 DIM Y[4999]            'Y[ ] holds the output signal
170 DIM H[100]             'H[ ] holds the filter kernel
180 '
190 PI = 3.14159265
200 FC = 0.1               'The cutoff frequency (0.1 of the sampling rate)
210 M% = 100               'The filter kernel length 
220 '
230 GOSUB XXXX             'Subroutine to load X[ ] with the input signal
240 '
250 '                      'CALCULATE THE FILTER KERNEL
260 FOR I% = 0 TO 100
270    IF (I%-50) = 0 THEN H[I%] = 2*PI*FC
280    IF (I%-50) <> 0  THEN H[I%] = SIN(2*PI*FC * (I%-50)) / (I%-50)
290    H[I%] = H[I%] * (0.54 - 0.46*COS(2*PI*I%/100) )
300 NEXT I%
310 '               
320                        'FILTER THE SIGNAL BY CONVOLUTION
330 FOR J% = 100 TO 4999      
340    Y[J%] = 0                   
350    FOR I% = 0 TO 100
360       Y[J%] = Y[J%] + X[J%-I%] * H[I%]
370    NEXT I%
380 NEXT J%
390 '
400 END
--}
fc = 500 -- cutoff frequency

hh = U.replicate 100 (0::Double) -- empty filter kernel

fKernel :: SamplesR -> SamplesR
fKernel hs = f hs (0::Int) (0::Double)
  where
    g v j = v *  (0.54 - 0.46*cos(2*pi*j/100))
    f h 100 _ = h

    f h 50 j = 
      let val = 2*pi*fc in
      f ((U.//) h [(50, g val j)]) 51 51

    f h i j =
      let val = sin(2*pi*fc * (j-50)) / (j-50) in
      f ((U.//) h [(i, g val j)]) (i+1) (j+1)

lowPass :: VectSamples -> VectSamples
lowPass samples = -- 7000 0.01
  let xx = (U.map fromIntegral samples)::SamplesR in
  let yy = xx in -- output vector

  U.map floor (f xx yy (fKernel hh) 100)
  where
    f x y h j | j == U.length x = y
              | otherwise =
                (U.//) y [ (j, ((U.!) y j) + ((U.!) x (j-i)) * ((U.!) h i)) | i <-[0..99]]


