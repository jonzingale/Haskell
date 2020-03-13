## Banking and Budgeting in Haskell

### Useful Docker methods
- docker system prune -a
- docker images
- docker build -t banking .
- docker run -it --rm banking bash
- docker run --name banking --mount source=Banking,target=/Banking -it bash

### Useful Stack method
- stack setup
- stack init
- stack build
- stack install # builds and more
- stack solver # installs new deps with resolver

### May need to install directly
stack install cassava
stack install extra
stack install HTF


### Updating Packages
vagrant halt
vagrant up
vagrant ssh
cd lowline_haskell
docker system prune -a
dude start


### Running a Library Method
```
stack ghci src/BankParser.hs
example date
```