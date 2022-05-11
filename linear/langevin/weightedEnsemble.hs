module WeightedEnsemble where
import Dynamics

{--
Weighted Ensemble is a search strategy for finding rare events, or perhaps a
method for learning distributions with limited resources.

It is effectively a plant metaphor, a diffusion limited aggregate
whose body is coaxed by underlying Langevin dynamics.

Monads:
- Giry
- State

Independence:
- Dynamics: Langevin
- Binning: static or adaptive
- Ensemble runs: learning distributions
--}

type Name = Int
type Prob = (Int, Int)

data Bin = B Name Prob
data Trajectory = T Int Int Prob -- 2D coords and a probability
