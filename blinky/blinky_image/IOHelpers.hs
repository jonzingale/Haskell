module IOHelpers where
import OneDimensional
import TwoDimensional
import Comonad

-- IO
seqn :: [IO a]-> IO()
seqn[] =return()
seqn (a:as) = do
  a
  seqn as

--clears the screen
cls :: IO()
cls = putStr "\ESC[2J"

--wait performs a given number of dummy actions 
wait :: Int -> IO()
wait n = seqn [return () | _ <- [1..n]]

life :: V Int -> Rule -> IO()
life b rule = do
  cls
  putStr $ show b
  wait $ 109060 * 10
  life (blink rule b) rule

-- Helpers
ruleToNumber :: [Int] -> Int
ruleToNumber as = foldr (\x y -> 2^x * y) 1 as - 1

numberToRule :: Int -> [Int]
numberToRule 0 = []
numberToRule n = numberToRule (div n 2) ++ [mod n 2]