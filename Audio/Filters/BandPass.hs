{--
function [B,A] = boost(g,fc,bw,fs);
%BOOST - Design a digital boost filter at given gain g, 
%        center frequency fc in Hz,
%        bandwidth bw in Hz (default = fs/10), and
%        sampling rate fs in Hz (default = 1).

if nargin<4, fs = 1; end
if nargin<3, bw = fs/10; end

c = cot(pi*fc/fs); % bilinear transform constant
cs = c^2;  csp1 = cs+1; Bc=(bw/fs)*c; gBc=g*Bc;
nrm = 1/(csp1 + Bc); % 1/(a0 before normalization)
b0 =  (csp1 + gBc)*nrm;
b1 =  2*(1 - cs)*nrm;
b2 =  (csp1 - gBc)*nrm;
a0 =  1;
a1 =  b1;
a2 =  (csp1 - Bc)*nrm;
A = [a0 a1 a2];
B = [b0 b1 b2];

if nargout==0
  figure(1);
  myfreqz(B,A); % /l/mll/myfreqz.m
  dstr=sprintf('boost(%0.2f,%0.2f,%0.2f,%0.2f)',g,fc,bw,fs);
  subplot(2,1,1); title(['Boost Frequency Response: ',...
                      dstr],'fontsize',24);
end
--}

module Filters.BandPass where
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

-- function [B,A] = boost(g,fc,bw,fs);
-- %BOOST - Design a digital boost filter at given gain g, 
-- %        center frequency fc in Hz,
-- %        bandwidth bw in Hz (default = fs/10), and
-- %        sampling rate fs in Hz (default = 1).
g = 2
fc = 0.25
bw = 0.1 --  samplingRate / 10 -- 1/10
fs = 1 -- samplingRate -- 1

bandPass :: VectSamples -> VectSamples
bandPass samples = -- 7000 0.01
  let ss = (U.map fromIntegral samples)::SamplesR in
  U.map floor (f ss 2)
  where
    f s i | i == U.length s - 2 = s
          | otherwise =
      let c = 1 / (tan(pi*fc/fs)) in
      let (cs, csp1, bc, gBc) = (c^2, cs+1, (bw/fs)*c, g*bc) in
      let nrm = 1/(csp1 + bc) in

      let b0 =  (csp1 + gBc)*nrm in
      let b1 =  2*(1 - cs)*nrm in
      let b2 =  (csp1 - gBc)*nrm in
      let a0 =  1 in
      let a1 =  b1 in
      let a2 =  (csp1 - bc)*nrm in

      let [p0, p1, p2] = [(U.!)] <*> [s] <*> [i, i-1, i-2] in
      let t = (b0 / a0 * p0) + (b1 / a0 * p1) + (b2 / a0 * p2) -
              (a1 / a0 * p1) - (a2 / a0 * p2) in
      f ((U.//) s [(i, t)]) (i+1)
