## Functorial Logic

This library is an exercise in developing logic from the perspective of
an ajoint situation:

∃p, ∀p :: P(X x Y) -> P(X) and ()* :: P(X) -> P(X x Y), ∃p |- p* |- ∀p


### Future Directions
- Extend to non-Boolean Toposes, for instance, category of irreflexive graphs.
  - Define relevant power object or explict Truth object.

- Develop classical propositional logic via product construction. See 6.6 of
Marquis below, page 224.
  - ∃yT(x,y) = ∃pT = { x 𝜀 X | ∃y(x,y) 𝜀 T }

- Consider unboxed Vectors and more efficient data structures (BTrees perhaps)
for calculating inverse images.

- Rely on laziness, evaluate only when necessary.

### References
- _From a Geometrical Point of View_; Jean-Pierre Marquis
- _Elementary Categories, Elementary Toposes_; Colin McLarty
- _Sets for Mathematics; William Lawvere_; Robert Rosebrugh
- _Categories for the Working Mathematician_; Saunders MacLane