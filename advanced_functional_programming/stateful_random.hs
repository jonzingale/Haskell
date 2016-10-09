{-- fastuous' idea.
  wrap randomness in a stateful way.
  use replicate to produce stateful
  randomness. Unpack the randomness
  and see that replicate did not just
  repeat a single value.
--}

import Control.Monad.State
import System.Random

randInt :: State StdGen Int
randInt = state (randomR (0,3))

randList :: State StdGen [Int]
randList = replicateM 10 randInt

nums :: [Int]
nums = evalState randList (mkStdGen 1)


-----
