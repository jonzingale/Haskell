# Number Theoretic Discrete Dynamical Systems

Some years ago I had a discussion with Bill Lawvere where he mentioned that he and Stephen Schanuel were working on a book about *Objective Number Theory*. I know as little now as I did then as to what he had in mind[1], but it did inspire me to think about how the concepts and tools of number theory could be extended to categories of interest beyond the arithmetic of numbers. Traditionally, number theory begins by developing the division operator over the ring of Integers and studying the rich consequences that follow. In this project, I limit my scope to the study of discrete dynamical systems (DDS) with the goal in mind to develop analogs to the Euclidean algorithm, the prime number theorem, gcd, arithmetic functions, mobius inversion, quadratic reciprocity, chinese remainder theorem and many other concepts central to elementary number theory.

The project begins with a review of the category of discrete dynamical systems and then a study of the combinatorial classification of DDS, their morphisms, and univeral constructions.

[1] I suspect he may have meant something quite opposite of what I understood. Perhaps something closer to recognizing objects and morphisms in standard number theory as a way to classify, study and understand traditional number theory. Categories of arithmetic functions and mobius inversion, for instance.

## Why DDS, Why Endomorphisms of Discrete Sets?
- endomorphisms are domains, like (co-)monads
- automata are interesting

## How Many/ How Big?
- pullbacks, pushforwards, sections, retractions, equilizers, hom

## Notable Properties and Provable Propositions:
- non-unique factorization
- DDS density of primes increase with n
- products of DDS are tensor products of matrices
- there exist n^n DDS on n states
- DDS are representable by adjacency matrices with a 1 in each row and 0 otherwise.
- the ergodic states (cycles) are a subcategory inclusion whose cofree left adjoint computes spectra.
- generating the n^n DDS can be produced efficiently from lists of base n Integers.
- DDS have exponential objects and are in-fact a topos.

## Toward Counting Morphisms:
1. cycles map to cycles and these will determine a good deal of the possible mappings.
  - n isomorphisms
  - multiplication principle per cyclic component
  - 

## Partial-Formed Questions:
- Does there exist another adjoint collapsing cycles to trivial cycles?
- Do Monad/Comonad contructions offer insight/leverage? (∐⊣Δ,Δ⊣∏)
- How do non-unique factors interact with other DDS, is there a metric?
- What kind of ring is DDS? (terminal DDS is unit, DDS is commutative, non-UFD)
- How do roots (cycles) transform under tensor products?

## Adjoints
- Ergodic cofree and left to inclusion (InnerErg), remove all transient states
- C: Dyn -> OuterErg, collapse all cycles to points right to inclusion
- G: OuterErg -> Dyn taking transients with points to an object of all component dyns that are isomorphic under cycles collapsing to points.
FG: OuterErg -> OuterErg taking an object to an infinite number of that same object.

```

    OuterErg
incl ⊣ collapse ⊣ N
     Dyn

  InnerErg
spectra ⊣ incl
    Dyn

units and counits are isos.
```