`ghci SpringRank/Test.hs`

Todo (Dev):
1. CsvParser -> Models/Graph.hs
2. proper cabal package
3. abstract tournament rankings in Test.hs

Todo (Sci):
1. generate minimal graph (tournament suggestion)
2. reason through handicap/open comparisons (thermal coupling)
 - players may change rankings between
 - some players may be in one but not the other
 - how reasonable is it to compare open versus handicap?
3. validate matchings influence of graph on fixed subgraph
4. generalize diffElo to include negative elo: eloDiff1 + eloDiff2
5. validate scaling by pairwise multiplication
6. jupyter notebook explaining calculations