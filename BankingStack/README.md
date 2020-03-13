# BankingStack

### Build and Run tests
- create `data/one_year.csv` and be sure that it has rows that look like:

`02/29/2020,Description of some transaction,-$4.19,$1234.17`

then:

```
stack build
stack build --test # runs the test suite @ test/Spec.hs
```

### Run Tests alone
`stack test`

### To enter ghci with loaded module
```
stack ghci BankParser.hs
```

### Warning:
In `package.yaml` the ghc_option `-F -pgmF htfpp` is set to play nice with Docker.
It appears that this flag breaks the same code running locally.