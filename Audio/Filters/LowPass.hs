module Filters.LowPass where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)

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

type SamplesR = U.Vector Double
type VectSamples = U.Vector Int32

fc = 0.01 -- cutoff frequency (0.1 of the sampling rate)
(mm, mm') = (300::Int, 300::Double)

blackman v j m = (* v) $ 0.42 - 0.5*cos(2*pi*j/m) + 0.08*cos(4*pi*j/m)
hamming v j m = (* v) $ 0.54 - 0.46*cos(2*pi*j/m)

kerh :: SamplesR
kerh = normalize $ U.generate (mm+1) (g.fromIntegral)
  where
    normalize h = U.map (/ (U.sum h)) h

    g j | j == mm'/ 2 = blackman (2*pi*fc) j mm'
        | otherwise =
          let val = sin(2*pi*fc * (j-mm'/2)) / (j-mm'/2) in
          -- blackman val j mm'
          hamming val j mm'

lowPass :: VectSamples -> VectSamples
lowPass samples =
  let xx = (U.map fromIntegral samples)::SamplesR in
  let padded = (U.++) (U.replicate mm (0::Double)) xx in
  U.map floor $ U.drop mm $ U.generate (U.length xx) (f padded kerh)
  where
    f x h j = sum [(U.!) x (j+mm-i) * (U.!) h i | i<-[0..mm]]
