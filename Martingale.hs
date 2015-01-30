module Martingale (purplemartin,drunkbet,cls) where
import System.Random
type Cozy = StdGen

--Martingales
mkBlanket :: Int -> Cozy
mkBlanket cozy = mkStdGen cozy

cls :: IO()
cls = putStr "\ESC[2J"

purplemartin :: IO()
purplemartin = 
	     do cls
	        putStr "Set your Bank" --Enter a number
		putChar '\n'
		sq<-getLine
		putStr ("You have "++sq++" squid.")
		putChar '\n'
		l 32 (read sq) 1 (0,0)
 where l s m g (wins,games)  =
     	do putStr "(h)eads or (t)ails?" --Hit an 'h' or 't' to play.
	   putChar '\n'
	   ys<-getLine
	   cls
	   if ['h','t']!!(fst(randomR (0,1) (mkBlanket s))) == head ys  
	     then do
	             putStr ("fuck yeah, "++(show (m+g))++" squid left")
	    	     putChar '\n'
		     putStr ("you're averaging "++(show (wins+1))++" wins out of "
			++(show (games+1))++" games")
		     putChar '\n'
		     putChar '\n'			    
		     l ((fst.next.mkBlanket) s) (m+g) 1 (wins+1,games+1)
	     else 
		if (m-g)<=0
		 then do putStr "BROKE ASS BANK. You best head home, son."
			 putChar '\n'
	         else do
	     	         putStr ("dang, "++(show (m-g))++" squid left")
		         putChar '\n'
		         putStr ("you're averaging "++(show wins)++" wins out of "
			  ++(show (games+1))++" games")	
		         putChar '\n'
		         putChar '\n'
		         putStr ("How about double or nothing? Make "++
				(show (2*g))++" times the standard chip!")
		         putChar '\n'
		         l ((fst.next.mkBlanket) s) (m-g) (2*g) (wins,games+1)

drunkbet :: IO()
drunkbet   = do cls
	        putStr "Set your Bank"
		putChar '\n'
		sq<-getLine
		putStr ("You have "++sq++" squid")
		putChar '\n'
		lime 32 (read sq)
 where lime s m  = do putStr "(h)eads or (t)ails?"
		      putChar '\n'
		      ys<-getLine
		      cls
		      if ['h','t']!!(fst(randomR (0,1) (mkBlanket s))) == head ys  
		       then do putStr ("fuck yeah, "++(show (m+1))++" squid left")
		               putChar '\n'			       
			       lime ((fst.next.mkBlanket) s) (m+1)			       
		       else if (m-1)<=0
			     then do putStr "BROKE ASS BANK. You best head home, son."
			             putChar '\n'
		             else do putStr ("dang, "++(show (m-1))++" squid left")
			             putChar '\n'
			             lime ((fst.next.mkBlanket) s) (m-1)

