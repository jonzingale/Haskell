module IOHelpers where
import Comonad
import TwoDimensional

run2d' = life rboard conway

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
