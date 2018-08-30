module Filters.ComonadicFilters where
import Prelude hiding ((=<<))
import Data.Int (Int32)

type FilterKernel = [Double]
type VectSamples = [Int32]
type CutOffFreq = Double
type Q = Double

{--
Here in it is shown that convolution of kernels used
to implement dsp filters are comonadic in nature.
--}

-- DSP section
lowPassW :: CutOffFreq -> VectSamples -> VectSamples
lowPassW fc vs = convolve vs (hh fc)

mm = 100::Int -- really ought to be 4/bw for vals 0..0.5

convolve :: VectSamples -> FilterKernel -> VectSamples
convolve xs hs = let xx = incl (map fromIntegral xs) in
  map floor $ returnM (length xs) $ xx =<< (conKer hs)
  where
    conKer h xs = sum $ zipWith (*) (reverse h) (returnM mm xs)

hh :: CutOffFreq -> FilterKernel
hh fc = normalize $ [ g.fromIntegral $ j | j<-[0..mm+1] ]
  where
    normalize h = map (/ (1 + sum h)) h
    -- g j = (fejer fc j (fromIntegral mm)) * blackman j (fromIntegral mm)
    g j = (sinc fc j (fromIntegral mm)) * blackman j (fromIntegral mm)
    blackman j m = 0.42 - 0.50*cos(2*pi*j/m) + 0.08*cos(4*pi*j/m)

    sinc f j m | j == m/2 = 2*pi*f/44100 -- Dirichlet
               | otherwise = sin(2*pi*f/44100 * (j-m/2)) / (j-m/2)

    fejer f j m | j == m/2 = 2*pi*f/44100-- Fejér
                | otherwise = let (n, x) = (2*pi*f/44100, j - m/2) in
                  (1/n) * ((1- cos(n*x)) / (1 - cos x))



-- Comonad section
data U x = U x [x]

right (U b []) = U b []
right (U b (c:cs)) = U c cs

instance Functor U where
  fmap f (U b c) = U (f b) (map f c)

class Functor w => Comonad w where
  (=<<)    :: w a -> (w a -> b) -> w b
  coreturn :: w a -> a
  cojoin     :: w a -> w (w a)
  x =<< f = fmap f (cojoin x)

instance Comonad U where
   cojoin a = U a (tail $ iterate right a)
   coreturn (U b _) = b

incl :: Num a => [a] -> U a
incl (x:xs) = U x (xs ++ repeat 0)

returnM :: Int -> U Double -> [Double]
returnM m (U x xs) = take m (x:xs)
