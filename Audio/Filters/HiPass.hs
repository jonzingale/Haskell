{--
/* High Pass Filter based on RBJ Cookbook linked above */
/* Analog Transfer Function for this filter: H(s) = s^2 / (s^2 + s/Q + 1) */

/* These floating point values are used by the filter code below */
float Fs = 44100;      /* sample rate in samples per second */
float Pi = 3.141592;   /* the value of Pi */

/* These floating point values implement the specific filter type */
float f0 = 100;                /* cut-off (or center) frequency in Hz */
float Q = 1.5;                 /* filter Q */
float w0 = 2 * Pi * f0 / Fs;
float alpha = sin(w0) / (2 * Q);
float a0 = 1 + alpha;
float a1 = -2 * cos(w0);
float a2 = 1 - alpha;
float b0 = (1 + cos(w0)) / 2;
float b1 = -(1 + cos(w0));
float b2 = (1 + cos(w0)) / 2;

/* The Buffer[] array holds the incoming samples, PrevSample[] holds intermediate results */
float Buffer[1024];         /* this array holds 1024 elements numbered 0 through 1023 */
float PrevSample[3];        /* this array holds 3 elements numbered 0 through 2 */

/* These integer (whole number) variables are used below
/* to process 1,024 iterations at a time*/
int I = 0;
int N = 1024;

/* The code below executes repeatedly as long as the value of I is less than N */
/* Since I was initialized to 0 above, and N was set to 1024, this code executes 1,024 times */
while (I < N) {                    /* this is the beginning of the code that loops 1,024 times */

  PrevSample[2] = PrevSample[1];   /* Slide the samples over one position */
  PrevSample[1] = PrevSample[0];
  PrevSample[0] = Buffer[I];

  Buffer[I] = ( b0 / a0 * PrevSample[0]) +
  (b1 / a0 * PrevSample[1]) +
  (b2 / a0 * PrevSample[2]) -
  (a1 / a0 * Buffer[I - 1]) -
  (a2 / a0 * Buffer[I - 2]);

I = I + 1;      /* increment the counter I by adding 1 */
}               /* this is the end of the code loop */
--}

module Filters.HiPass where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import System.Random

type SamplesR = U.Vector Double
type VectSamples = U.Vector Int32

randos :: VectSamples
randos = (U.fromList).(take 44100) $ rs
  where rs = randomRs (minBound, maxBound::Int32) $ mkStdGen 23

fs = 44100; -- /* sample rate in samples per second */
f0 = 7000 -- /* cut-off (or center) frequency in Hz */
q = 0.01  -- /* filter Q */
w0 = 2 * pi * f0 / fs
alpha = (sin w0) / (2 * q)
a0 = 1 + alpha
a1 = -2 * cos w0
a2 = 1 - alpha
b0 = (1 + cos w0) / 2
b1 = -(1 + cos w0)
b2 = (1 + cos w0) / 2

-- Eventually parallelize, 1024 say. bucket brigade threes.
-- Is this form a linear recurrence? f n = a*f(n-1) + b*f(n-2)
hiPass :: VectSamples -> VectSamples
hiPass samples = 
  let ss = (U.map fromIntegral samples)::SamplesR in
  U.map floor (f ss 2)
  where
    f s i | i == U.length s - 2 = s
          | otherwise = 
      let [p0, p1, p2] = [(U.!)] <*> [s] <*> [i, i-1, i-2] in
      let t = (b0 / a0 * p0) + (b1 / a0 * p1) + (b2 / a0 * p2) -
              (a1 / a0 * p1) - (a2 / a0 * p2) in
      f ((U.//) s [(i, t)]) (i+1)
