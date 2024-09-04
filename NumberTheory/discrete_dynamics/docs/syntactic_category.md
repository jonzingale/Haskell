- https://ncatlab.org/nlab/show/syntactic+category
- https://www.youtube.com/watch?v=L6LPEFteLts
- https://www.youtube.com/watch?v=VgzcAKVdfic&t=405s

# Syntactic Category

## TODO:
- https://www.youtube.com/watch?v=L6LPEFteLts generic models provide point-free constructions.

## Introduction:
The __syntactic category__, or category of __contexts__, is a functor `Con: Type Theories -> Modeling Categories`, equipped with a right adjoin `Lang: Modeling Categories -> Type Theories` recovering the *internal logic* of a given model.

That is, there exists an adjoint relationship between categories of __type theories__ and categories of __models__ whose *universal constructions* and *natural transformations* provide insight into long confronted philosophical problems and their associated concepts.

For instance, a *type theory* may characterize a given theory of computation by providing a framework via __terms of given types__, __judgements__, and __rules of inference__ for expressing the *intension* of a given program or computation.

The functor `Con: Theories -> Models` then *wraps* a given theory into its various *extensions*, providing the theory an embodiment embedded within a system of *contexts*.

Dually, there exists a functor `Lang: Models -> Theories` that gives the *internal logic* of a given model. `Con` and `Lang` are an adjoint pair, left and right respectively.

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
- ε relates a model of models to it's prototype model. Game of Life written in Game of Life, for instance.

- η: Let T be a theory of computation that is *wrapped* in the context as a __generic or universal__ computer, Con(T). Arrows from Con(T) to another model of computation, Game of Life say, are interpretations such as Conway's original interpretation (gates built with Gosper gliders). η then gives the unique way the theory of computation T is logically given in the theory of Game of Life.

- w ~ Con∘Lang: Subcategory of generic models?
