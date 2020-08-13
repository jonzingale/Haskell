##Comonadic Cellular Automata##

The data type `V x` is thought of as a bundle with a connection:

`data V = V [U x] (U x) [U x]` where `data U = U [x] x [x]` are fibers.

###Todo:###
- tease out into separate folders: 1d-toy, 2d-toy, production
- optimize production code via parallelization and vector types
- blinky images: CA and convolution