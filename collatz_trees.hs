module CollatzTrees where

fibs :: Integer -> [Integer]
fibs n | n < 2 = [n]
			 | otherwise = fibs(n-1) ++ fibs(n-2)

choose _ 0 = [1]
choose n k | n < k = [0]
 				   | otherwise = (choose (n-1) k) ++ (choose (n-1) (k-1))

-- these keeps recursive history.
tibs n xs | n == 0 = " o" ++ xs
          | n == 1 = " *" ++ xs
			 		| otherwise = (tibs (n-1) ("f"++xs)) ++ (tibs (n-2) ("g"++xs))

-- keeps the correct history.
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
								 	 | p == 'g' = col 'f' (n-1) $ (fs.ff)

fs n = n/2
gs n = 3*n+1

another_answer = [js k > k| k<-[1..100], js <- (jensf 5)]
better n = [js 500 > 500| js <- jensf n]

-- now evaluate and sort these strings based on the collatz.
eval_collatz xs | eval xs 32 > 32 = "diverge"
								| otherwise = "converge"
	where eval (x:xs) n | x == '*' = n
											| x == 'f'  = eval xs (n/2)
											| otherwise = eval xs (3*n+1)

some_answers n = zip (jens n) $ map eval_collatz  $ jens n