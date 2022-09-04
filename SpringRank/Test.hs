module Test where
import SpringRank (springRank)

normalize :: [(Int, Double)] -> [(Int, Double)]
normalize ls = let k = 1 / (maximum.map snd $ ls) in
  [(l, k * val) | (l, val) <- ls]

rasmus_test = do
  edge <- springRank "data/edge.dat"
  putStr "*-*: "
  print $ normalize edge

  two_edge <- springRank "data/two_edge.dat"
  putStr "*-*-*: "
  print $ normalize two_edge

  weighted_two_edge <- springRank "data/weighted_two_edge.dat"
  putStr "*-*-*: "
  print $ normalize weighted_two_edge

  three_edge <- springRank "data/three_edge.dat"
  putStr "*-*-*-*: "
  print $ normalize three_edge

rasmus_test2 = do
  putStrLn "\nWhen can three edges have the same value as two?"
  putStrLn "ie. When is the sum of the middle values in the three case"
  putStrLn "equal to the middle in the two case?\n"

  weighted_two_edge <- springRank "data/weighted_two_edge.dat"
  let w_two = normalize weighted_two_edge
  putStr "*-*-*: "
  print $ map snd w_two

  weighted_three_edge <- springRank "data/weighted_three_edge.dat"
  let w_three = normalize weighted_three_edge
  putStr "*-*-*-*: "
  print $ map snd w_three

  let wte1 = snd $ w_three!!1
  let wte2 = snd $ w_three!!2
  let te = snd $ w_two!!1

  putStr "with a difference of: "
  print $ wte2 + wte1 - te

rasmus_test3 = do
  putStrLn "\nWhen is the drop across two edges the same as across one?"

  kirchoff <- springRank "data/kirchoff2.dat"
  let k = normalize kirchoff
  -- print $ map snd k
  putStrLn.unlines $ map show k
