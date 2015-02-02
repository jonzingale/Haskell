-- Here is an attempt to make a blinky game for Haskell
-- >>= is the Then operator
-- Do the commands like \ESC[2J not work with this version of Haskell? they do
-- however 'mod' doesn't seem to work out for creating infix situation. why not?
-- life glider runs blinkylight with glider setup
module Blinkylight where
import Data.Char
import Data.List
import Data.Time
import SortsShuffles
import System.Random

width :: Int
width = 260
height :: Int
height =50

type Board = [Pos]
type Pos = (Int,Int)

cls :: IO()
cls = putStr "\ESC[2J"
--}
writeat :: Pos -> String -> IO()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO()
goto (x,y) = putStr ("\ESC[" ++ show y++";"++show x++"H")

seqn :: [IO a]-> IO()
seqn[] =return()
seqn (a:as) = do a
                 seqn as

--Some Initial Conditions
-- (x,y) where x is horizontal from left and y is vertical from top
glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

--gliderfest ::Int-> Board
--gliderfest n = [ (a+r,b+r')|(a,b)<-glider,(r,r')<-collage n] 

--collage :: Int->Board
--collage n = let as= boredoms n     in
--	    let bs= boredoms (n^2) in
--	  [ (((`mod`width)a),((`mod`height)b)) |(a,b)<-zip as bs]

tenner :: Board
tenner = let x = div width 2
             y = div height 2
         in [(x,y),(x,y+1),(x,y+2),(x,y+3),(x,y+4),
             (x,y+5),(x,y+6),(x,y+7),(x,y+8),(x,y+9)]



band :: Board
band = [(x,y) |x<-[1..width],y<-[div height 2]]

expband :: Board
expband = [(x,y) |x<-[1..width],y<-[div height 2]] 
                ++ [(x+div width 2,y+div height 2) |x<-[1..4],y<-[1]]

pentaboo :: Board
pentaboo = let x = div width 2
               in [(x,10),(x,11),(x,12),(x+1,10),(x-1,11)]

random_board :: Int -> Board
random_board s = let rs = ((randomRs (0,div width 2)).mkBlanket) in
                nub [(x,y)|(x,y)<-((take (7*width)).zip (rs s)) (rs (s+1)) ]

-- Display the cells and decide their state
showcells :: Board -> IO ()
showcells b = seqn [writeat p "o" | p <- (knuffle b)]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

--Who are my neighbors?
neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),
                          (x+1,y-1),(x-1,y),
                          (x+1,y)  ,(x-1,y+1),
                          (x,y+1)  ,(x+1,y+1)]
--wrap gives us a torus
wrap :: Pos -> Pos
wrap (x,y) = ((mod (x-1) width)+1,(mod (y-1) height)+1)

--who is alive?
liveneighbs :: Board -> Pos -> Int
liveneighbs b = length.filter (isAlive b).neighbs
 
--Rules Rules Rules
survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p)[2,3]]

births :: Board -> [Pos]
--births b = [(x,s) | x <- [1..width],
--                    y <- [1..height],
--                    isEmpty b (x,y),
--                    liveneighbs b (x,y) == 3]

--Since this inefficiently considers all lets do only neighbors of living
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]
--rmdups removes duplicates from the list to insure each counted once
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/=x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

--life implements the game itself: clears screen, considers situation,
--waits a moment and lastly updates

life :: Board -> IO()
life b = do cls
            showcells b
            wait 109060
            life (nextgen b)

--wait performs a given number of dummy actions 
wait :: Int -> IO()
wait n = seqn [return () | _ <- [1..n]]
