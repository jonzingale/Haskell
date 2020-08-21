module Helpers where
import Control.Monad.State
import System.Random
import DLA

listboard :: [[Int]]
listboard = f.(take 100) $ map g randos
  where
    f [] = []
    f ls = take 10 ls : (f.drop 10 $ ls) 
    randos = randoms $ mkStdGen 99 :: [Int]
    g x = case (x `mod` 20) of
      0 -> 1
      _ -> 0

-- Writer Monad

output :: Int -> Board -> State [Bound] Board
output n bd = do
  put.bounds $ bd
  case n of
    0 -> return bd
    _ -> output (n-1) (blink 12 bd)

-- returnState :: [Bound]
-- returnState =
  -- let (n, state) = runState $ output 12 board in state

{--
Are we getting any closer?
runState (output 12 board) [P 0 0]
--}