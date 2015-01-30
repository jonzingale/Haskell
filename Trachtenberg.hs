module Trachtenberg where
type Z = Integer
type N = Z

cls :: IO()
cls = putStr "\ESC[2J"

walk = [1..]
{-- are to numbers as ... are to lists
    note: numbers place values right to left
cls clears
long like length
leave n remaining
lose like drop
grab like take
end like last
start like fst
rest like tails
?? like !!
cons like :

--Trachtenberg Multiplication Algorithm
arm
arms
trachtenberg
  --
trachtenburgh: falls apart when the answer contains zeros
conns
------
 

--}

long :: Integral a=>a->Z
long n = glen n 0
  where  glen z i| z `mod` 10^(fromIntegral i) == z = i
	  	 | otherwise = glen z (i+1)

leave :: Integral a=>Z->a->a
leave i n = n `div`10^(long n -fromIntegral i)
lose :: Integral a=>Z->a->a
lose i n = n `div` 10^fromIntegral i
grab :: Integral a=>Z->a->a
grab i n = n `mod` 10^fromIntegral i

end :: Integral a=>a->a
end n = leave 1 n
start :: Integral a=>a->a
start n = grab 1 n
rest ::Integral a=>a->a
rest n = lose 1 n

(??) :: (Integral a,Num a)=>a->Z->a 
(??) n i | i<0 = 0
	 | otherwise= grab 1 (leave (long n -fromIntegral i) n)

cons :: N->N->N
cons i 0 = i*10
cons i n = i*10^long n + n

--actually I am not sure this can be done without lists.
--as there is no symbol for numbers larger than 9.
tobase :: N->N->N --inverse back to base to needed.
tobase i j= bonk i j 0
 where
 bonk i 0 n = 0
 bonk i j n = j`mod`i*10^n+bonk i (j`div`i) (n+1)

--this shit is all wrong!
b2ten :: N->N->N
b2ten b 0 = 1
b2ten b n = b2ten b (n`div`b) + (n`mod`b )

{--
pasca11 :: N->N
pasca11 n= ( (b2ten n).(^n).(tobase (n*5`div`2)))n
--}

{--
Note/Warning:
the above use of `like`, as in
grab is `like` take, is necessary
as the functor doesn't completely
apply. For instance, grab 2 004
doesn't return 00 nor does 
leave 2 203 return 03. These
functions are at best `like` 
their list related cousins.

generality isn't quite right.
fromIntegral i should happen 
at every instance of i.
--}

arm::N->N->N->N
arm n m i = let tens = sub(i-1) n in
	    let ones = sub i n in
       ((ones??i) *m)??0 + ((tens??(i-1)) *m)??1
 where
  sub j z| or[j<0,j>long z]==True = 0
	 | otherwise = z

arms :: N->N->N->N->(N,N)
arms n m memory i=
   let crum = sum[ arm n (m??j) (i-j) |j<-[0..(long m -1)]]+memory in
	  if crum>9 then (end crum,start crum) else (0,start crum)

{--
It's funny how much slower trachtenbergs method
is for a computer and yet that much faster for a human.
The algorithm minimizes space complexity at the expense
of time.
--}
trachtenberg :: N->N->N
trachtenberg nom mon= recurse nom mon (long nom+long mon) "" 0
 where
  recurse n m 0 answer mem = read answer
  recurse n m l store mem = let (s,t)=arms n m mem (long n+long m-l) in
 				recurse n m (l-1) (show t++store) s 

---
trachtenburgh:: N->N->N
trachtenburgh nom mon= recurse nom mon (long nom+long mon) 0 0
 where
  recurse n m 0 answer mem = answer
  recurse n m l store mem = let (s,t)=arms n m mem (long n+long m-l) in
 				recurse n m (l-1) (t`conns`store) s 

conns :: N->N->N
conns i n = i*10^long n + n

{--
With this function I got so close to not 
having to refer to lists at any point.
Then in the end it all got botched up
by the inability to cleanly and clearly
cons a 0 to the front of a number.
Somehow both Bogus and funny.

Maybe this particularity points 
at something deep about the structure
of representable numbers, maybe
it's just not that interesting.
I don't know?
--}
---

