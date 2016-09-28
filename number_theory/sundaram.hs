import Data.List
-- :set +s

oddify x = 2*x + 1

limit lim = (div lim 2) - 2
j_limit lim = div (limit lim - 1) 3
i_limit lim j = div (lim - 2 - 2*j) (4*j+2) 

composite_cores lim = [i+j+2*i*j | j<-[1..j_limit lim], i<-[1..i_limit lim j]]
sundaram lim = map oddify $ [1..limit lim] \\ composite_cores lim
main = putStrLn $ show $ sundaram (10^3)

{--
instead of array increment and print, but how?
the goal would be to insert by place value like in ruby code.
ary = (2..@limit/2).map{|t| 2*t - 1}
comp [1..odd_limit] j_lim i_lim lim
odd_limit lim = div (limit lim) 2

something to check out would be dictionaries from:
import qualified Data.Map.Lazy as Map

Map.insert 5 't' (Map.fromList [(5,'a'), (3,'b')])

ideally, i imagine i would want to do something
like have nils to place in the correct spaces a' la:
comp_cores lim = [ary[i+j+2*i*j-1] | j<-[1..j_limit lim], i<-[1..i_limit lim j]]
but I am unsure how to do this in a haskell context.
--}

sieve lim = let j_lim = j_limit lim in
  comp [1..limit lim] (j_lim) (i_limit lim j_lim) lim

  where
      comp xs 0 _ l = map oddify xs
      comp xs j 0 l = comp xs (j-1) (i_limit l j) l
      comp xs j i l = comp (delete (i+j+2*i*j) xs) j (i-1) l
