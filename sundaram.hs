-- def sundaram # 10**8 max
--   limit  = @limit/2 - 2
--   j_limit = (limit-1)/3

--   # only odd are needed.
--   ary = (2..@limit/2).map{|t| 2*t - 1}

--   (1..j_limit).each do |j|
--     (1..j).each do |i|
--       num = i + j + 2*i*j - 1
--       break if num > limit
--       ary[num] = nil
--     end
--   end
--   ary.compact.unshift 2
-- end

-- ary =[] ; (1..100).each{|j|(1..j).map{|i| ary<<[j,i,i+j+2*i*j]}}
--ary.sort_by!{|i,j,k|k}
--ary.map{|t|puts t.to_s};nil

import Data.List
-- :set +s
oddify x = 2*x + 1
limit lim = (div lim 2) - 2
j_limit lim = div (limit lim - 1) 3

-- fast but not that fast. try recusion
composite_cores :: Int -> [Int]
composite_cores lim = [i+j+2*i*j | j<-[1..j_limit lim], i<-[1..j]]

-- sorting composite_cores is just a bit faster!!
sundaram lim = map oddify $ [1..limit lim] \\ composite_cores lim

main = putStrLn $ show $ sundaram (10^3)

