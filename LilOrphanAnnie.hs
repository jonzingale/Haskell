
{-- 
Little Orphan Annie Comonadic
Secret Decoder Ring App
--}

module LilOrphanAnnie (encode,lil'orphan) where
import Comonad
import Data.Char
import Pprimes

type Distr = [Float]
type Message = String

--how many steps should a message take to decode?
lil'orphan annie = do chisit annie

chiSeed = message_lift sometext

sometext :: Message
sometext = "ftue tme dqmxxk nqqz eayq etuf"
encodedText = encode (-12) sometext

message_lift :: Message -> U Distr
message_lift = (periodU.prism)

prism :: Message -> Distr
prism = freqs

eng :: Distr
eng = engFreqTable 

periodU :: [a] -> U [a]
periodU ds = U (lrep ds) ds (rrep ds)
	where
	 lrep (x:xs) = (xs++[x]) : lrep(xs++[x])
	 rrep xs = let (b:bs) = reverse xs in
		   let she = reverse(bs++[b]) in
			she : rrep she

chi :: U Distr -> Distr
chi u   | ((x2.coreturn.left) u) < ((x2.coreturn) u) 
			= (chi.left) u
	| otherwise = coreturn u

x2 os = sum [((e-o)^2)/e|(e,o)<-zip eng os]

--can this code be made faster by randomizing the 
--U Distr ?

chisit message =  
	  putStr $
          unlines $ 
	  (take 40) $
          map (groit.toList (0) 1) $
          map (morph message (message_lift message))$ 		
          iterate (=>> chi)(message_lift message)

--randomizing the U DISTR of an input
--seems considerably less efficient.
rChisit message =  
	  putStr $
          unlines $ 
	  (take 20) $
          map (groit.toList (0) 1) $
          map (morph message (message_lift message))$ 		
          iterate (=>> chi) ((message_lift message)=>>randi)


morph st ns ms= periodU $encode (-(distance ns ms)) st

groit :: (Show a) => [a]->String
groit = twostr
 where
  twostr [] =  " "
  twostr (n:ns) = (show n) ++ twostr ns


			------from Hutton
encode :: Int->String->String
encode n xs = [sshift n x | x<- xs]

sshift :: Int -> Char -> Char
sshift n c | isLower c = int2let (mod (let2int c+n) 26)
          | otherwise = c

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

engFreqTable :: [Float]
engFreqTable = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,
                6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]

percent :: Int->Int->Float
percent n m = ( toEnum n/ toEnum m) * 100

freqs :: String -> [Float]
freqs xs = [percent(count x xs) n | x<- ['a'..'z']]
            where n = lowers xs

count :: Char -> String -> Int
count x xs = length [x' | x'<-xs, x == x']

lowers :: String -> Int
lowers xs = length [x| x<-xs, isLower x]

