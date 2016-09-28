#!/usr/bin/runhaskell
import Blinkylight
import Data.Time
main = do b <- getCurrentTime;
			 let g = (floor.utctDayTime) b in
			 (life.random_board) g