module Birthday where

import Data.Time.Calendar
import Data.Time

pop = (fromGregorian 1942 2 18)
ali = (fromGregorian 1977 8 23)
jon = (fromGregorian 1980 5 10)
zeke = (fromGregorian 1981 11 6)
sarah_j = (fromGregorian 1985 5 14)
katie = (fromGregorian 1987 6 27)

daysold name = do   putChar '\n';
			        (y,m,d) <- fmap (toGregorian.localDay.zonedTimeToLocalTime) getZonedTime;
			        let p = diffDays (fromGregorian y m d) name in
			        let str1 = show p ++ " days old, a " in 
							let str2 = if prime p then "Prime." else "Composite." in
			        let next = show $ (head [i |i<-[p..],prime i])-p in
			        let msg = foldr(++) "" 
			        		[str1,str2,"\nThe next prime day is in ",next," days \n\n"] in
			        putStr$msg;

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
