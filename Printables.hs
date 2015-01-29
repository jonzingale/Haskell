module Printables  where
import System.Random
import Data.Char


type Board = [Pos]
type Pos = (Int,Int)

writeat :: Pos -> String -> IO()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO()
goto (x,y) = putStr ("\ESC[" ++ show y++";"++show x++"H")

seqn :: [IO a]-> IO()
seqn[] =return()
seqn (a:as) = do a
                 seqn as

showcells ::Show a => Int -> [a] -> IO ()
showcells i b = do {--cls--}; seqn [writeat (i,j) (show p) | (j,p) <- zip [1..] b];putChar '\n'

-- IO ()
printit [] = putStrLn ""
printit (x:xs) = do (putStrLn.show) x ;
					 printit xs

cls :: IO()
cls = putStr "\ESC[2J"

walk :: Int -> [Int]
walk n = [1..n]