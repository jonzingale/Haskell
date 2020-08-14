module DLA where

{--
Diffusion limited aggregation
https://en.wikipedia.org/wiki/Diffusion-limited_aggregation

To one perceptual approximation, one may model the dependence on histories
relative to the path of individual particles. From another perspective, the
dependence on histories may be interpreted in terms of overall 'board state',
leading to a fairly natural poset interpretation.

Algebraically, it seems to me that DLAs can be nicely characterized as a
comonad on a category of pointed spaces[.], the aggregate acting as the
adjoined 'point'. Then under actions of the coalgebra we get transitions
on the space, with the aggregates mapping to aggregates as 'basepoint'
preservation.

Here I plan to flesh this idea out with a combination of a Cellular Automata
comonad and Maybe (possibly State or Writer) monad.
--}

-- randomwalk: look in a patch, if 1 then select random neighbor.