module FreeVector where

{--
Part of the idea here is to understand
the counit for a free vector construction
over a finite field.


counit :: FG K -> K
1a + 0b + 0c -> a
0a + 1b + 0c -> b
0a + 0b + 1c -> c

and the rest follows some how.
--}

data K v = K v

