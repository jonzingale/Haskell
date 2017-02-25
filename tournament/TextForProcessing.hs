module TextForProcessing where
import Tournament

{--
a module for writing Graphs to a text file so they can
be visualized in RubyProcessing.
--}

file_path = "./../../RubyProcessing/tournaments/lists.txt"

-- writeFile :: FilePath -> String -> IO ()
it = appendFile file_path "Some much awesome in the world today.\n"

toTxt s = do
  let tourns = [show.tournament i $ 12 | i <- key_shuffle s [1..15] ] ;
  writeFile file_path $ unlines tourns

likely_best =  [(12,11),(12,10),(12,9),(8,7),(8,6),(8,5),(4,3),
                (4,2),(4,1),(11,10),(11,9),(7,6),(7,5),(3,2),(3,1),
                (10,9),(6,5),(2,1)]
