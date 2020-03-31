## Spring Rank

This library is a Haskell implementation of the spring rank algorithm
developed by researchers at the Santa Fe Institute. The original python
code base and preprints can be found [HERE](https://github.com/cdebacco/SpringRank).

### To compile, run and cleanup:
`ghc -O2 -o springrank Main.hs`

`time ./springrank`

`rm *.hi *.o`

### Benefits:
- Amenable to compilation and compiler optimization.
- Extendable to Cuda via `accelerate`.

### TODO:
- Optimize with unboxed vectors rather than lists.
- Extend to accept `alpha`, `l0` and `l1` params like the original.
- Extend to accept non-`Int` tokens.