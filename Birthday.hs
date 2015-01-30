module Birthday where

import Data.Time.Calendar
import Data.Time

ali = (fromGregorian 1977 8 23)
jon = (fromGregorian 1980 5 10)
sarah_j = (fromGregorian 1985 5 14)

daysold name = do   putChar '\n';
			        (y,m,d) <- fmap (toGregorian.localDay.zonedTimeToLocalTime) getZonedTime;
			        (putStr.(++ " days old, a ").show) $ diffDays (fromGregorian y m d) name;
			        putStr $ if prime (diffDays (fromGregorian y m d) name) then "Prime." else "Composite.";
			        let p = (diffDays (fromGregorian y m d) name) in
			        let next = head [i|i<-[p..],prime i] in
			        putStr $ "\nMy next prime day is in "++show(next - p)++" days \n\n";

sarahPrimes = do c <- getCurrentTime;
							let z = (utctDay) c in
							let diffs x = diffDays (addDays x z) sarah_j in
							let primes = [(addDays d z, diffs d) |d<-[0..364], (prime.diffs) d] in
							(putStr.unlines.map show) primes

jonPrimes = do c <- getCurrentTime;
					let z = (utctDay) c in
					let diffs x = diffDays (addDays x z) jon in
					let primes = [(addDays d z, diffs d) |d<-[0..364], (prime.diffs) d] in
					(putStr.unlines.map show) primes

aliPrimes = do c <- getCurrentTime;
					let z = (utctDay) c in
					let diffs x = diffDays (addDays x z) ali in
					let primes = [(addDays d z, diffs d) |d<-[0..364], (prime.diffs) d] in
					(putStr.unlines.map show) primes



prime p = ffactors p == [1]
intToFloat n = fromInteger (toInteger n)
ffactors n = let xs = [1..(floor.sqrt.intToFloat) n] in 
             [ x  | x <- xs, n`mod`x == 0 ]

todo = path++"/Birthday.hs"
path = "/Users/jonzingale/Desktop/crude/Haskell"

--add a Birthday to the list below
addBirthday =
	do putStr $ "You will need to reload :r after to use new birthday"++"\n"
	   putStr $ "Name of person"++"\n"
	   name_of <- getLine
	   putStr $ "year of birth"++"\n"
	   year_of <- getLine
	   putStr $ "numeric month of birth"++"\n"
	   month_of <- getLine
	   putStr $ "numeric day of birth"++"\n"
	   day_of <- getLine
	   appendFile 
	    todo
	      ( name_of ++ "= (fromGregorian "++year_of++" "++month_of++" "++day_of++")" )




----- New Birthdays
