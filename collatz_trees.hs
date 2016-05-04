module CollatzTrees where

-- keeps history.
jens n = f '*' n ""
	where f p n xs | n == 0 = [p:xs]
								 | p == '*' = f 'f' (n-1) (p:xs) ++ f 'g' (n-1) (p:xs)
								 | p == 'f' = f p (n-1) (p:xs) ++ f 'g' (n-1) (p:xs)
								 | p == 'g' = f 'f' (n-1) $ p:xs

-- keeps history returns composition.
jensf n = col '*' n id
	where col p n ff | n == 0 = [id.ff]
								 	 | p == '*' = col 'f' (n-1) (fs.ff) ++ col 'g' (n-1) (gs.ff)
								 	 | p == 'f' = col p (n-1) (fs.ff) ++ col 'g' (n-1) (gs.ff)
								 	 | p == 'g' = col 'f' (n-1) (fs.ff)

fs n = n/2
gs n = 3*n+1

another_answer = [js k > k| k<-[1..100], js <- (jensf 5)]
better n = [js 500 > 500| js <- jensf n]

-- now evaluate and sort these strings based on the collatz.
eval_collatz xs | eval xs big_num > big_num = "D"
								| otherwise = "C"
	where eval (x:xs) n | x == '*' = n
											| x == 'f'  = eval xs (n/2)
											| otherwise = eval xs (3*n+1)

some_answers n = zip (jens n) $ map eval_collatz  $ jens n
big_num = 32^20

convergent n = ((length.filter it) (some_answers n), (length.some_answers) n)
	where it n = "C" == snd n


feel_em = [fromIntegral x/ fromIntegral y|(x,y)<-ratios]
ratios = [(1,2),(1,3),(4,5),(5,8),(6,13),(17,21),(23,34),(50,55),(73,89),(103,144),(211,233),(314,377),(581,610),(895,987),(1350,1597),(2455,2584),(3805,4181),(5798,6765),(10395,10946),(16193,17711),(27875,28657),(44068,46368),(68940,75025),(118013,121393),(186953,196418),(312974,317811),(499927,514229),(793453,832040),(1325204,1346269),(2118657,2178309),(3368279,3524578),(5612906,5702887),(8981185,9227465),(14797581,14930352),(23778766,24157817),(38078927,39088169),(62674883,63245986),(100753810,102334155),(164736197,165580141)]

foldrr op z [] = z
foldrr op z (x:xs) = op x $ foldrr op z xs 

--- working with lines
jens_ary str = innerfun (reverse str) [1,1,0]
	where
		innerfun [] abc = abc
		innerfun (x:xs) (a:b:c:[]) | x == '*' = innerfun xs [a, b, c]
														 	 | x == 'f' = innerfun xs [a/2, b, c]
														 	 | x == 'g' = innerfun xs [a, 3*b, 3*c+1/a]

-- evalStr ((jens 5)!! 4) 12 => 16.0
evalStr str n = eval (jens_ary str) n
	where eval (a:b:c:[]) n = a * (n*b + c)

--ruby line equations
--it derives from jens_ary
--it.map{|a,b,c| "#{b.to_i}/#{(1/a).to_i} + #{c.to_i}/#{b.to_i}"}



--data W = Int | Diverge
ary2weight (a:b:c:[]) = ceiling $ (1-b*a)/(c*b)






