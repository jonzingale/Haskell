#!/usr/bin/runhaskell
import Birthday
main = do
        putStr "\nTycho";
        daysold tycho;
        putStr "\nJon";
        daysold jon;
        putStr "\nSarah";
        daysold sarah_j;
        putStr "\nPop";
        daysold pop;

-- ghc -o goodMorning birthdayScript.hs
