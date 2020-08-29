module Helpers where
import Control.Monad.State (runState)
import System.Random (mkStdGen)
import DLA

{--
Pass (blinkStates n board) an initial seed
and get nth board state.
--}

ex1 n = runState (blinkStates n board) (mkStdGen 12)
