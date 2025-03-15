## References:
- https://ncatlab.org/nlab/show/syntactic+category (read the references)
- https://www.youtube.com/watch?v=L6LPEFteLts (type theory topology, pointfree)
- https://www.youtube.com/watch?v=VgzcAKVdfic&t=405s (extension-intension)
- https://ncatlab.org/nlab/show/abstract+general%2C+concrete+general+and+concrete+particular (general, particular, abstract, ...)
- https://arxiv.org/abs/1811.00420 behavioral mereology
- https://golem.ph.utexas.edu/category/2019/06/behavioral_mereology.html
- https://www.researchgate.net/figure/Contravariant-Mapping-between-Intensional-universe-left-and-Extensional-universe-right_fig5_228926485
- https://www.researchgate.net/publication/228926485_The_Universe_as_a_freely_generated_Information_System

-  Lawvere, F W, Adjointness in Foundations (extension-intension)
- https://scienceblogs.com/goodmath/2006/08/10/from-lambda-calculus-to-cartes (Useful)

# Syntactic Category

## TODO:
- https://www.youtube.com/watch?v=L6LPEFteLts generic models provide point-free constructions.
- How do programming languages fit in? Hask the cat, Python, general-particular? SAT-satisfaction proof.
- Can being in Syntactic categories be interpreted as Hegel, Nietzche, Deleuze? Can we have a logic that is not a logic of parts?
- Logic is contra to Geometry, Intension contra to extension, mereology
- What do universals buy me? when η is an iso for instance?

## Introduction:
The __syntactic category__, or category of __contexts__, is a functor
`Con: Type Theories -> Modeling Categories`, equipped with a right adjoin
`Lang: Modeling Categories -> Type Theories` recovering the *internal logic*
of a given model.

That is, there exists an adjoint relationship between categories of
__type theories__ and categories of __models__ whose *universal constructions*
and *natural transformations* provide insight into long confronted philosophical
problems and their associated concepts.

For instance, a *type theory* may characterize a given theory of computation by
providing a framework via __terms of given types__, __judgements__, and
__rules of inference__ for expressing the *intension* of a given program or
computation.

The functor `Con: Theories -> Models` then *wraps* a given theory into its
various *extensions*, providing the theory an embodiment embedded within a
system of *contexts*.

Dually, there exists a functor `Lang: Models -> Theories` that gives the
*internal logic* of a given model. `Con` and `Lang` are an adjoint pair,
left and right respectively.

## Consequences of Con ⊣ Lang

T ------------> Lang M

Con T -----> Con∘Lang M
                |
                | ε
                ⋁
                M

Con T ------------> M

Lang∘Con T -----> Lang M
    ⋀
    | η
    |
    T

where:
- ε relates a model of models to it's prototype model. Game of Life written in
Game of Life, for instance.

- η: Let T be a theory of computation that is *wrapped* in the context as a
    __generic or universal__ computer, Con(T). Arrows from Con(T) to another model
    of computation, Game of Life say, are interpretations such as Conway's original
    interpretation (gates built with Gosper gliders). η then gives the unique way
    the theory of computation T is logically given in the theory of Game of Life.

- "Con(Lan(C)) → C says that there a canonical interpretation of the internal
    logic of a category C in C itself"

- w ~ Con∘Lang: Subcategory of generic models?
