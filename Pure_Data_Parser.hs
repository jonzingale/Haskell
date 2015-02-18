module Pure_Data_Parser where
import System.Random
import Printables
import SortsShuffles
import Typehandling

type Z = Integer
type Prime = Integer
type Base = Z
type Rand = Z

-- an example of how pure data files should be parsed.
pure_data_parse :: String -> [(Char,Z)]
pure_data_parse ary = index ary 0 [0]
		where	
				index [] _ _ = []
				index (x:xs) n (r:rs) | x == 'N' = (x,n) : index xs (n+1) (n+1:r:rs)
															| x == 'X' = (x,n-1) : index xs n (r:rs)
															| x == 'R' = (x,r-1) : index xs n rs



bary = ['N','N','X','N','X','R','R','N','X','R','R']