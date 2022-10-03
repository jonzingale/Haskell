module Birthday where
import Data.Numbers.Primes
import Text.Printf
import Data.Time.Calendar
import Data.Time

pop = (fromGregorian 1942 2 18)
ali = (fromGregorian 1977 8 23)
jon = (fromGregorian 1980 5 10)
zeke = (fromGregorian 1981 11 6)
sarah_j = (fromGregorian 1985 5 14)
tycho = (fromGregorian 2020 3 3)
katie = (fromGregorian 1987 6 27)
grandmaT = (fromGregorian 1928 6 18)

daysold :: Day -> IO ()
daysold name =
  do  (y, m, d) <- fmap gregorianLocal getZonedTime
      putStr.format_birthday (y, m, d) $ name
  where
    gregorianLocal = toGregorian.localDay.zonedTimeToLocalTime

format_birthday :: (Integer, Int, Int) -> Day -> String
format_birthday (y, m, d) name =
  let p = toInteger.diffDays (fromGregorian y m d) $ name in
  printf template p (primeTmp p) (nextPrime p)
  where
    template = "\n%d days old, a %s.\nThe next prime day is in %d days\n\n"
    primeTmp p | isPrime p = "Prime"
               | otherwise = "Composite"
    nextPrime p = (head [i | i <- [p+1..], isPrime i]) - p

namedPrimes :: Day -> IO ()
namedPrimes name =
  do c <- getCurrentTime;
     let z = (utctDay) c
     let diffs x = diffDays (addDays x z) name
     let primes = [(addDays d z, diffs d) |d<-[0..364], (isPrime.diffs) d]
     (putStr.unlines.map show) primes
