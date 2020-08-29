module Helpers where
import Control.Monad.State (runState)
import System.Random (mkStdGen)
import DLA

-- pass (blinkStates n) an initial seed and get nth board state
ex1 n = runState (blinkStates n) (mkStdGen 12)
