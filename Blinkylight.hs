-- Here is an attempt to make a blinky game for Haskell
-- >>= is the Then operator
-- Do the commands like \ESC[2J not work with this version of Haskell? they do
-- however 'mod' doesn't seem to work out for creating infix situation. why not?
-- life glider runs blinkylight with glider setup
module Blinkylight where
import Data.Char
--import Test
import SortsShuffles
import System.Random

width :: Int
width = 26
height :: Int
height =40

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




-- Display the cells and decide their state
showcells :: Board -> IO ()
showcells b = seqn [writeat p "X" | p <- (knuffle b)]

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
            wait 5060
            life (nextgen b)

--wait performs a given number of dummy actions 
wait :: Int -> IO()
wait n = seqn [return () | _ <- [1..n]]



{--


-----------1 Dimensional Automata:
type Rule = Int
type State = [Int]
--some Seeds
empty::[a]
empty = []

n216 :: [Integer]
n216 = ones 216

ninety1s :: [Integer]
ninety1s = ones 90

scattered :: String->[Integer]
scattered word = (toBin.fst.randomR (0,2^196)) ((mkBlanket.length) word)

sizable :: Integer->[Integer]
sizable n = shuffle ((ones (n`div`2))++(zeros (n`div`2)))

biased :: Integer->Integer->[Integer]
biased one zero = shuffle (ones one++(zeros zero))

---------- 

newmexico :: (Integral a)=>[a]->Rule->IO()
newmexico binyseed roos = 
  let quo= (incl8.map fromIntegral) binyseed in  
     do cls
	seqn[writeat (0,i) (take 220 q)| (i,q)<- zip walk (blink roos quo)]
	putChar '\n'	

blink :: Rule->State->[String]
blink roos quo =
   (showstate (transition roos quo)):(blink roos (transition roos quo))

transition :: Rule->State->State
transition roos st =
     	1: [ ((incl8.toBin) roos)!!n | n<-(train roos (st++[0,0,0]) [])]
      where
       train rz (a:b:[c]) st' = st' 
       train rz (a:b:c:zs) st'= train rz (b:c:zs) (st'++[bin2Int [a,b,c]])

showstate :: State->String
showstate [] = ""
showstate (st:quo) = ([" ","`"]!!st)++(showstate quo)


--
charch :: String->String
charch [] = ""
charch (n:ns)| (ord n)==49 = " "++(charch ns)
charch (n:ns)|otherwise = "`"++(charch ns)

bin2Int :: Integral a=> [a] -> a-- bin2int [1,1,0] -> 3
bin2Int = foldr (\x y->x+2*y) 0
toBin :: (Integral a)=>a -> [a] --binar 6 = [0,1,1]
toBin 0 = []
toBin n = (mod n 2):toBin(div n 2)--[0,1,1,0,0,0,0,0]
incl8 :: Integral a=>[a]->[a]
incl8 ns | length ns>=8  =ns
	 | otherwise = incl8 (ns++[0])

zeros :: (Integral a)=>a->[a]
zeros n = [0*k|k<-[1..n]]
walk :: (Integral a)=> [a]
walk = [0..]
--}
