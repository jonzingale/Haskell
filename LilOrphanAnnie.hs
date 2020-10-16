
{-- 
Little Orphan Annie Secret Comonadic
Decoder Ring for Ceasar Ciphers
--}

module LilOrphanAnnie (encode,lil'orphan) where
import Control.Comonad
import Data.Char

type Distr = [Float]
type Message = String

lil'orphan annie = do chisit $ map toLower annie

chiSeed = message_lift sometext

sometext :: Message
sometext = "gung erzvaqf zr gung v'ir ybfg zl pnrfne pbqr cyhtva! v'yy unir gb erjevgr vg."

-- takes a message and returns a U type
message_lift :: Message -> U Distr
message_lift = periodU.freqs

periodU :: [a] -> U [a]
periodU ds = U (lrep ds) ds (rrep ds)
  where
    lrep (x:xs) = (xs++[x]) : lrep(xs++[x])
    rrep xs = let (b:bs) = reverse xs in
              let she = reverse(bs++[b]) in
              she : rrep she

chi :: U Distr -> Distr
chi u | ((chisqr.extract.left) u) < ((chisqr.extract) u) = (chi.left) u
      | otherwise = extract u

chisqr :: [Float] -> Float
chisqr os = sum [ (e-o)^2 /e | (e,o) <- zip engFreqTable os]

chisit message =  
  putStr.unlines.take 26 $
  map (groit.toList 0 1) $
  map (morph message (message_lift message)) $
  iterate (=>> chi)(message_lift message)

morph st ns ms = periodU $ encode (-(distance ns ms)) st

groit :: (Show a) => [a] -> String
groit = twostr
 where
  twostr [] =  " "
  twostr (n:ns) = (show n) ++ twostr ns

------from Hutton
encode :: Int -> String -> String
encode n xs = map (sshift n) xs

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

percent :: Int -> Int -> Float
percent n m = ( toEnum n/ toEnum m) * 100

freqs :: String -> [Float]
freqs xs = [ percent (count x xs) (length xs) | x <- ['a'..'z']]

count :: Char -> String -> Int
count x xs = length [x' | x'<-xs, x == x']

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode factor $ map toLower xs
  where
    factor = head $ positions (minimum chitab) chitab
    chitab = [chisqr (rotate n table') | n <- [0..25]]
    table' = freqs xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..n], x==x']
                  where n = length xs -1
---- alphabet as a group
class Periodic p where
  rotateR :: p a -> p a

-- geez what can i do to create this class?
instance Periodic U where
  rotateR (U as b (c:cs)) = U (b:as) c cs 

---------
data U x = U [x] x [x]

instance Show a => Show (U a) where
  show (U a b c) = show (a++[b]++c)

right (U a b (c:cs)) = U (b:a) c cs
left  (U (a:as) b c) = U as a (b:c)

instance Functor U where
  fmap f (U a b c) = U (map f a) (f b) (map f c)

instance Comonad U where
  duplicate a = U (tail $ iterate left a) a (tail $ iterate right a)
  extract (U _ b _) = b

---- distance for zippers

repeatedAlpha = concat $ repeat ['a'..'z']

distance ::(Ord a) =>U a -> U a -> Int
distance u v  =
  let pairs = takeUntil (cotequal (extract v)) $ zip (map extract(iterate left u)) (map extract(iterate right u)) in
    (sign(extract v) (last pairs)) * ((length pairs)-1)

  where 
  cotequal c (a,b) = and [a/=c,b/=c]
  sign c (a,b) | c==a = 1
               | otherwise = -1
  takeUntil b xs= take (((length.takeWhile b) xs)+1) xs

shift i u = (iterate (if i<0 then left else right) u) !! abs i

toList i j u = take (j-i) $ half $ shift i u where
    half (U _ b c) = [b] ++ c

