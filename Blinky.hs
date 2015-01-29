module Blinky (Seed,Rule,newmexico,cls) where
import Data.Char
import System.Random
-----------1 Dimensional Automata:
type Rule = Int
type State = [Int]
type Seed = Integer
type Cozy = StdGen

--newmexico 2 82
newmexico :: Seed->Rule->IO()
newmexico seed roos = 
  let binyseed = biased seed 120 in
  let quo= (incl8.map fromIntegral) binyseed in  
     do cls
	seqn[writeat (0,i) (take 220 q)| (i,q)<- zip walk (blink roos quo)]
	putChar '\n'

-----------

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


n2Int n = foldr (\x y->x+n*y) 0 -- why just binaries?
bin2Int :: Integral a=> [a] -> a-- bin2int [1,1,0] -> 3
bin2Int = foldr (\x y->x+2*y) 0
toBin :: (Integral a)=>a -> [a] --binar 6 = [0,1,1]
toBin 0 = []
toBin n = (mod n 2):toBin(div n 2)--[0,1,1,0,0,0,0,0]
incl8 :: Integral a=>[a]->[a]
incl8 ns | length ns>=8  =ns
	 | otherwise = incl8 (ns++[0])

----helpers
cls :: IO()
cls = putStr "\ESC[2J"
zeros :: (Integral a)=>a->[a]
zeros n = [0*k|k<-[1..n]]
walk :: (Integral a)=> [a]
walk = [0..]
ones :: (Integral a)=>a->[a]
ones 0 = []
ones n = 1:(ones (n-1)) 
biased :: Integer->Integer->[Integer]
biased one zero = shuffle (ones one++(zeros zero))

----mkPrintable
writeat :: (Int,Int) -> String -> IO()
writeat p xs = do goto p
                  putStr xs
goto :: (Int,Int) -> IO()
goto (x,y) = putStr ("\ESC[" ++ show y++";"++show x++"H")
seqn :: [IO a]-> IO()
seqn[] =return()
seqn (a:as) = do a
                 seqn as

----Rand
shuffle :: Ord a => [a]->[a]
shuffle xs = (snd.unzip.rQsort)(zip ((boredoms.length) xs) xs)
boredoms :: Int -> [Int] 
boredoms zola = take zola (randoms (mkBlanket zola))
rQsort :: Ord a => [a] -> [a]
rQsort eart = (qsort.snd.unzip.qsort) (zip (boredoms (length eart)) eart)
mkBlanket :: Int -> Cozy
mkBlanket cozy = mkStdGen cozy
qsort :: Ord a => [a]->[a]  
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
           where
             smaller = [s | s<-xs, s<=x]
             larger  = [l | l<-xs, l > x]




