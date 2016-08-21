import Data.List
-- :set +s

oddify x = 2*x + 1

limit lim = (div lim 2) - 2
j_limit lim = div (limit lim - 1) 3
i_limit lim j = div (limit lim - j) (2*j + 1)

-- fast but not that fast. try recusion
composite_cores lim = [i+j+2*i*j | j<-[1..j_limit lim], i<-[1..i_limit lim j]]

-- sorting composite_cores is just a bit faster!!
sundaram lim = map oddify $ [1..limit lim] \\ composite_cores lim

main = putStrLn $ show $ sundaram (10^3)

