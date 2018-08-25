{--
100 'BAND-PASS WINDOWED-SINC FILTER
110 'This program calculates an 801 point band-pass filter kernel
120 '
130 DIM A[800] 'A[ ] workspace for the lower cutoff
140 DIM B[800] 'B[ ] workspace for the upper cutoff
150 DIM H[800] 'H[ ] holds the final filter kernel
160 '
170 PI = 3.1415926
180 M% = 800 'Set filter kernel length (801 points)
190 '
200 ' 'Calculate the first low-pass filter kernel via Eq. 16-4,
210 FC = 0.196 'with a cutoff frequency of 0.196, store in A[ ]
220 FOR I% = 0 TO 800
230 IF (I%-M%/2) = 0 THEN A[I%] = 2*PI*FC
240 IF (I%-M%/2) <> 0 THEN A[I%] = SIN(2*PI*FC * (I%-M%/2)) / (I%-M%/2)
250 A[I%] = A[I%] * (0.42 - 0.5*COS(2*PI*I%/M%) + 0.08*COS(4*PI*I%/M%))
260 NEXT I%
270 '
280 SUM = 0 'Normalize the first low-pass filter kernel for
290 FOR I% = 0 TO 800 'unity gain at DC
300 SUM = SUM + A[I%]
310 NEXT I%
320 '
330 FOR I% = 0 TO 800
340 A[I%] = A[I%] / SUM
350 NEXT I%
360 ' 'Calculate the second low-pass filter kernel via Eq. 16-4,
370 FC = 0.204 'with a cutoff frequency of 0.204, store in B[ ]
380 FOR I% = 0 TO 800
390 IF (I%-M%/2) = 0 THEN B[I%] = 2*PI*FC
400 IF (I%-M%/2) <> 0 THEN B[I%] = SIN(2*PI*FC * (I%-M%/2)) / (I%-M%/2)
410 B[I%] = B[I%] * (0.42 - 0.5*COS(2*PI*I%/M%) + 0.08*COS(4*PI*I%/M%))
420 NEXT I%
430 '
440 SUM = 0 'Normalize the second low-pass filter kernel for
450 FOR I% = 0 TO 800 'unity gain at DC
460 SUM = SUM + B[I%]
470 NEXT I%
480 '
490 FOR I% = 0 TO 800
500 B[I%] = B[I%] / SUM
510 NEXT I%
520 '
530 FOR I% = 0 TO 800 'Change the low-pass filter kernel in B[ ] into a high-pass
540 B[I%] = - B[I%] 'filter kernel using spectral inversion (as in Fig. 14-5)
550 NEXT I%
560 B[400] = B[400] + 1
570 '
580 '
590 FOR I% = 0 TO 800 'Add the low-pass filter kernel in A[ ], to the high-pass
600 H[I%] = A[I%] + B[I%] 'filter kernel in B[ ], to form a band-reject filter kernel
610 NEXT I% 'stored in H[ ] (as in Fig. 14-8)
620 '
630 FOR I% = 0 TO 800 'Change the band-reject filter kernel into a band-pass
640 H[I%] = -H[I%] 'filter kernel by using spectral inversion
650 NEXT I%
660 H[400] = H[400] + 1
670 ' 'The band-pass filter kernel now resides in H[ ]
680 END
--}

module Filters.BandPass where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import System.Random

{--
http://www.analog.com/media/en/technical-documentation/dsp-book/dsp_book_Ch16.pdf
--}

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

bandPass :: VectSamples -> VectSamples
bandPass samples = -- Convolve the input signal & filter kernel
  let xx = (U.map fromIntegral samples)::SamplesR in
  let padx = (U.++) (U.replicate mm (0::Double)) xx in
  U.map floor $ U.drop mm $ U.generate (U.length xx) (f padx kerh)
  where
    f x h j = sum [(U.!) x (j+mm-i) * (U.!) h i | i<-[0..mm]]

