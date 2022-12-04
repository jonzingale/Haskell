## Ciphers

`cabal run Ciphers`

### cabal for profiling
1. cabal init
2. edit `ciphers.cabal` executables to include ghc options
and build dependencies

```
executable Ciphers
    -- Import common warning flags.
    import:           warnings

    -- .hs or .lhs file containing the Main module.
    main-is:          Main.hs

    ghc-options:
      -O2
      -threaded
      -prof
      -fprof-auto
      "-with-rtsopts=-N -p -s -hT -i0.1"

    -- Other library packages from which modules are imported.
    build-depends:
      base ^>=4.16.4.0,
      vector >= 0.13.0.0,
      bytestring >= 0.11.3.1
```

create `cabal.project.local` enabling profiling:
`cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks
`

which creates a file with:
```
ignore-project: False
library-profiling: True
executable-profiling: True
tests: True
benchmarks: True
```

`cabal build`
`cabal install --only-dependencies`
`cabal run Ciphers`