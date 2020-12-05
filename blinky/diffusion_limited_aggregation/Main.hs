module Main where
import Control.Monad.State
import System.Random
import DLAVector

-- . GenerateDLA.sh

main = do -- 7000 steps, 4000 particles, 400x400 in 40 secs
  let dla = runState (blinkStates 7000 board) (mkStdGen 12)
  print dla