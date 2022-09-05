## Spring Rank

This library is a Haskell implementation of the spring rank algorithm
developed by researchers at the Santa Fe Institute. The original python
code base and preprints can be found [HERE](https://github.com/cdebacco/SpringRank).

### To compile, run and cleanup:
`cabal install <necessary packages>`

`ghc -O2 -o springrank Main.hs`

`time ./springrank`

`rm *.hi *.o`

### Dependencies:
```
cabal install --lib vector-0.13.0.0
cabal install --lib cassava-0.5.3.0
cabal install --lib extra-1.7
cabal install --lib sparse-linear-algebra-0.3.1
cabal install --lib Unique-0.4.7.9
```

### Potential Benefits:
- Amenable to compilation and compiler optimization.
- Extendable to Cuda via `accelerate`.
